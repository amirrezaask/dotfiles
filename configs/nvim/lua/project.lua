--- Per-project Neovim context: cwd, buffer isolation, jumplist, persisted state.
---
--- State model: in-memory at runtime, persisted to disk as backup. Load once at setup,
--- flush periodically and on exit. Never re-read JSON on every operation.
---
--- Buffer/jumplist isolation: each project has its own buffer list and jumplist.
--- Switching projects saves current state, restores target state, isolates buffers.

---@class project.TerminalConfig
---@field position "split"|"vsplit"|"tab"|"float"
---@field split string
---@field size integer
---@field float_width integer
---@field float_height integer
---@field border string|nil

---@class project.Config
---@field projects_dir string
---@field max_depth integer
---@field store_path string|nil
---@field terminal project.TerminalConfig|nil

---@class project.StateFile
---@field version integer
---@field projects table<string, project.Entry>

---@class project.Entry
---@field last_file string|nil
---@field cursor { lnum: integer, col: integer }|nil
---@field jumplist project.JumplistState|nil

---@class project.JumpEntry
---@field file string
---@field lnum integer
---@field col integer
---@field coladd integer

---@class project.JumplistState
---@field pos integer|nil
---@field entries project.JumpEntry[]
---@field jumps_blob string|nil
---@field jumps_b64 string|nil

---@class project.PickerItem
---@field project string
---@field text string

local M = {}

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

M.STORE_VERSION = 1
M.DEFAULT_STORE_NAME = "project-state.json"
M.LEGACY_STORE_NAME = "project-last-files.json"
M.JUMPLIST_DEBOUNCE_MS = 1500
M.PERSIST_INTERVAL_MS = 5000

M.root_patterns = { ".git", "package.json", "go.mod", "Cargo.toml" }
M.fallback_names = { "README.md", "readme.md", "package.json", "go.mod", "Makefile" }

-- ---------------------------------------------------------------------------
-- Module state (in-memory, persisted to disk as backup)
-- ---------------------------------------------------------------------------

---@type project.Config
local config = {}

---@type string|nil Current active project root
local active_root = nil

---@type boolean Prevents re-entrant operations during switch
local lock = false

---@type table<string, integer> Primary terminal buffer per project
local primary_terminal = {}

---@type project.StateFile|nil In-memory state, loaded once at setup
local state = nil

---@type boolean True when state needs disk flush
local state_dirty = false

---@type uv_timer_t|nil Periodic persist timer
local persist_timer = nil

---@type integer Last jumplist persist timestamp (ms)
local last_jumplist_persist_ms = 0

-- ---------------------------------------------------------------------------
-- Path utilities
-- ---------------------------------------------------------------------------

---@param path string|nil
---@return string|nil
local function normalize_dir(path)
  if not path or path == "" then
    return nil
  end
  local normalized = vim.fn.fnamemodify(path, ":p")
  if type(normalized) ~= "string" then
    return nil
  end
  return (normalized:gsub("/+$", ""))
end

---@param path string|nil
---@return string|nil
local function normalize_file(path)
  local dir = normalize_dir(path)
  if not dir then
    return nil
  end
  if vim.fn.isdirectory(dir) == 1 then
    return nil
  end
  return dir
end

---@param file string|nil
---@param root string|nil
---@return boolean
local function file_in_root(file, root)
  if not file or not root then
    return false
  end
  return file == root or vim.startswith(file, root .. "/")
end

---@return string
local function store_path()
  if config.store_path then
    return config.store_path
  end
  return vim.fn.stdpath("data") .. "/" .. M.DEFAULT_STORE_NAME
end

---@return string
local function legacy_store_path()
  return vim.fn.stdpath("data") .. "/" .. M.LEGACY_STORE_NAME
end

---@return string|nil
local function get_projects_dir()
  return normalize_dir(config.projects_dir) or normalize_dir(vim.fn.expand("~/dev"))
end

-- ---------------------------------------------------------------------------
-- JSON persistence utilities
-- ---------------------------------------------------------------------------

---@param text string
---@return string
local function repair_json_twos_indent(text)
  return text:gsub("\n(2+)", function(twos)
    return "\n" .. ("  "):rep(#twos)
  end)
end

---@param text string
---@return table|nil decoded
---@return boolean repaired
local function decode_json_text(text)
  local ok, decoded = pcall(vim.json.decode, text)
  if ok and type(decoded) == "table" then
    return decoded, false
  end
  local fixed = repair_json_twos_indent(text)
  ok, decoded = pcall(vim.json.decode, fixed)
  if ok and type(decoded) == "table" then
    return decoded, true
  end
  return nil, false
end

---@param path string
---@return table|nil decoded
---@return boolean repaired
local function decode_json_file(path)
  if vim.fn.filereadable(path) ~= 1 then
    return nil, false
  end
  local lines = vim.fn.readfile(path)
  if #lines == 0 then
    return nil, false
  end
  return decode_json_text(table.concat(lines, "\n"))
end

-- ---------------------------------------------------------------------------
-- State normalization
-- ---------------------------------------------------------------------------

---@param raw table
---@return project.JumplistState|nil
function M.normalize_jumplist(raw)
  if type(raw) ~= "table" then
    return nil
  end

  ---@type project.JumplistState
  local result = {}

  if type(raw.jumps_blob) == "string" and raw.jumps_blob ~= "" then
    result.jumps_blob = raw.jumps_blob
  end
  if type(raw.jumps_b64) == "string" and raw.jumps_b64 ~= "" then
    result.jumps_b64 = raw.jumps_b64
  end

  if type(raw.entries) == "table" then
    local entries = {}
    for _, jump in ipairs(raw.entries) do
      if type(jump) == "table" and type(jump.file) == "string" and jump.lnum then
        local file = normalize_file(jump.file)
        if file then
          entries[#entries + 1] = {
            file = file,
            lnum = jump.lnum,
            col = jump.col or 0,
            coladd = jump.coladd or 0,
          }
        end
      end
    end
    if #entries > 0 then
      result.entries = entries
      local pos = tonumber(raw.pos)
      if not pos or pos < 1 or pos > #entries then
        pos = #entries
      end
      result.pos = pos
    end
  end

  if result.jumps_blob or result.jumps_b64 or result.entries then
    return result
  end
  return nil
end

---@param raw any
---@return project.Entry
function M.normalize_entry(raw)
  if type(raw) == "string" then
    return { last_file = raw }
  end
  if type(raw) ~= "table" then
    return {}
  end

  ---@type project.Entry
  local entry = {}

  if type(raw.last_file) == "string" then
    entry.last_file = raw.last_file
  end

  if type(raw.cursor) == "table" and raw.cursor.lnum then
    entry.cursor = {
      lnum = raw.cursor.lnum,
      col = raw.cursor.col or 0,
    }
  end

  if type(raw.jumplist) == "table" then
    entry.jumplist = M.normalize_jumplist(raw.jumplist)
  end

  return entry
end

---@return project.StateFile
local function empty_state()
  return {
    version = M.STORE_VERSION,
    projects = {},
  }
end

---@return project.StateFile
local function get_state()
  if not state then
    state = empty_state()
  end
  return state
end

-- ---------------------------------------------------------------------------
-- Project root detection
-- ---------------------------------------------------------------------------

---@param root string|nil
---@return boolean
function M.is_managed_root(root)
  root = normalize_dir(root)
  if not root then
    return false
  end

  local projects_dir = get_projects_dir()
  if not projects_dir then
    return false
  end

  if root ~= projects_dir and not vim.startswith(root, projects_dir .. "/") then
    return false
  end

  local detected = vim.fs.root(root, M.root_patterns)
  return detected ~= nil and normalize_dir(detected) == root
end

-- ---------------------------------------------------------------------------
-- State load/persist (disk backup)
-- ---------------------------------------------------------------------------

---@param projects table<string, any>
local function prune_unmanaged_projects(projects)
  local to_remove = {}
  for root in pairs(projects) do
    if not M.is_managed_root(root) then
      to_remove[#to_remove + 1] = root
    end
  end
  for _, root in ipairs(to_remove) do
    projects[root] = nil
  end
end

---@param raw table
---@return table<string, project.Entry>
local function migrate_legacy_projects(raw)
  local projects = {}
  for root, value in pairs(raw) do
    local key = normalize_dir(root) or root
    projects[key] = M.normalize_entry(value)
  end
  return projects
end

function M.load_state()
  local path = store_path()
  local decoded, repaired = decode_json_file(path)

  if decoded and decoded.version == M.STORE_VERSION and type(decoded.projects) == "table" then
    local projects = {}
    for root, raw in pairs(decoded.projects) do
      local key = normalize_dir(root) or root
      projects[key] = M.normalize_entry(raw)
    end
    prune_unmanaged_projects(projects)
    state = {
      version = M.STORE_VERSION,
      projects = projects,
    }
    if repaired then
      state_dirty = true
      vim.notify("Repaired corrupted project-state.json", vim.log.levels.INFO)
    end
    return
  end

  local legacy, legacy_repaired = decode_json_file(legacy_store_path())
  if legacy and not legacy.version then
    local projects = migrate_legacy_projects(legacy)
    prune_unmanaged_projects(projects)
    state = {
      version = M.STORE_VERSION,
      projects = projects,
    }
    state_dirty = true
    if legacy_repaired then
      vim.notify("Migrated legacy project-last-files.json", vim.log.levels.INFO)
    end
    return
  end

  state = empty_state()
end

---@param force? boolean
function M.persist_state(force)
  if vim.in_fast_event() then
    vim.schedule(function()
      M.persist_state(force)
    end)
    return
  end

  if not force and not state_dirty then
    return
  end

  local s = get_state()
  s.version = M.STORE_VERSION
  s.projects = s.projects or {}

  local path = store_path()
  local dir = vim.fn.fnamemodify(path, ":h")

  local ok_mkdir = pcall(vim.fn.mkdir, dir, "p")
  if not ok_mkdir then
    return
  end

  local ok_encode, encoded = pcall(vim.json.encode, s, { indent = "  ", sort_keys = true })
  if not ok_encode or type(encoded) ~= "string" then
    return
  end

  local ok_write = pcall(vim.fn.writefile, vim.split(encoded, "\n", { plain = true }), path)
  if ok_write then
    state_dirty = false
  end
end

local function start_persist_timer()
  if persist_timer then
    pcall(function()
      persist_timer:stop()
      persist_timer:close()
    end)
    persist_timer = nil
  end

  persist_timer = vim.uv.new_timer()
  if persist_timer then
    persist_timer:start(M.PERSIST_INTERVAL_MS, M.PERSIST_INTERVAL_MS, function()
      M.persist_state(false)
    end)
  end
end

---@param root string
---@return project.Entry
function M.get_project(root)
  root = normalize_dir(root)
  if not root then
    return {}
  end
  local s = get_state()
  return M.normalize_entry(s.projects[root])
end

---@param root string
---@param entry project.Entry
function M.set_project(root, entry)
  root = normalize_dir(root)
  if not root or not M.is_managed_root(root) then
    return
  end
  get_state().projects[root] = M.normalize_entry(entry)
  state_dirty = true
end

-- ---------------------------------------------------------------------------
-- Buffer root detection
-- ---------------------------------------------------------------------------

---@param bufnr integer
---@return string|nil
function M.path_root(bufnr)
  if not vim.api.nvim_buf_is_valid(bufnr) then
    return nil
  end

  local name = vim.api.nvim_buf_get_name(bufnr)
  if name == "" or name:match("^%w[%w%-]*://") then
    return nil
  end

  local path = vim.fn.fnamemodify(name, ":p")
  if vim.fn.isdirectory(path) == 1 or vim.fn.filereadable(path) == 1 then
    return vim.fs.root(path, M.root_patterns)
  end

  local parent = vim.fn.fnamemodify(path, ":h")
  if parent ~= "" and parent ~= path then
    return vim.fs.root(parent, M.root_patterns)
  end

  return nil
end

---@param bufnr integer
---@return string|nil
function M.buffer_root(bufnr)
  if not vim.api.nvim_buf_is_valid(bufnr) then
    return nil
  end

  local tagged = normalize_dir(vim.b[bufnr].project_root)
  if tagged and M.is_managed_root(tagged) then
    return tagged
  end

  local root = normalize_dir(M.path_root(bufnr))
  if root and M.is_managed_root(root) then
    return root
  end

  return nil
end

---@param bufnr integer
function M.assign_buffer_project(bufnr)
  if not vim.api.nvim_buf_is_valid(bufnr) then
    return
  end

  local root = normalize_dir(M.path_root(bufnr))
  if root and M.is_managed_root(root) then
    vim.b[bufnr].project_root = root
    return
  end

  if vim.b[bufnr].project_root and not M.is_managed_root(vim.b[bufnr].project_root) then
    vim.b[bufnr].project_root = nil
  end

  if vim.b[bufnr].project_root then
    return
  end

  local buftype = vim.bo[bufnr].buftype
  if buftype == "terminal" or (buftype == "" and vim.api.nvim_buf_get_name(bufnr) == "") then
    local current = M.current_root()
    if current and M.is_managed_root(current) then
      vim.b[bufnr].project_root = current
    end
  end
end

---@param bufnr integer
---@return boolean
function M.should_track(bufnr)
  if not vim.api.nvim_buf_is_valid(bufnr) then
    return false
  end
  if vim.bo[bufnr].buftype ~= "" then
    return false
  end
  local name = vim.api.nvim_buf_get_name(bufnr)
  if name == "" or name:match("^%w[%w%-]*://") then
    return false
  end
  return vim.fn.filereadable(name) == 1
end

-- ---------------------------------------------------------------------------
-- Jumplist capture/restore
-- ---------------------------------------------------------------------------

---@param jump table
---@return string|nil
local function jump_file_path(jump)
  local name = jump.filename
  if type(name) ~= "string" or name == "" then
    if not jump.bufnr or not vim.api.nvim_buf_is_valid(jump.bufnr) then
      return nil
    end
    name = vim.api.nvim_buf_get_name(jump.bufnr)
  end
  return normalize_file(name)
end

---@return project.JumplistState|nil
function M.capture_jumplist()
  local ok_jl, jumplist = pcall(vim.fn.getjumplist)
  if not ok_jl or type(jumplist) ~= "table" then
    return nil
  end

  local list = jumplist[1]
  local pos = jumplist[2]
  if type(list) ~= "table" then
    list = {}
  end
  pos = tonumber(pos) or #list

  local entries = {}
  local filtered_pos = 0
  for i, jump in ipairs(list) do
    if type(jump) == "table" then
      local file = jump_file_path(jump)
      if file and vim.fn.filereadable(file) == 1 then
        entries[#entries + 1] = {
          file = file,
          lnum = jump.lnum,
          col = jump.col or 0,
          coladd = jump.coladd or 0,
        }
        if i == pos then
          filtered_pos = #entries
        end
      end
    end
  end

  local jumps_blob
  local ok_ctx, ctx = pcall(vim.api.nvim_get_context, { types = { "jumps" } })
  if ok_ctx and type(ctx) == "table" and type(ctx.jumps) == "table" then
    local ok_enc, encoded = pcall(function()
      return vim.base64.encode(vim.mpack.encode(ctx.jumps))
    end)
    if ok_enc and type(encoded) == "string" and encoded ~= "" then
      jumps_blob = encoded
    end
  end

  if #entries == 0 and not jumps_blob then
    return nil
  end
  if filtered_pos == 0 and #entries > 0 then
    filtered_pos = #entries
  end

  ---@type project.JumplistState
  local captured = {}
  if #entries > 0 then
    captured.entries = entries
    captured.pos = filtered_pos
  end
  if jumps_blob then
    captured.jumps_blob = jumps_blob
  end
  return captured
end

---@param jumps_blob string
---@return boolean
local function load_jumps_blob(jumps_blob)
  local ok, chunks = pcall(function()
    return vim.mpack.decode(vim.base64.decode(jumps_blob))
  end)
  if not ok or type(chunks) ~= "table" then
    return false
  end
  pcall(vim.cmd.clearjumps)
  local ok_load = pcall(vim.api.nvim_load_context, { jumps = chunks })
  return ok_load
end

---@param pos integer|nil
local function restore_jump_position(pos)
  pos = tonumber(pos)
  if not pos or pos < 1 then
    return
  end
  local ok_jl, current = pcall(vim.fn.getjumplist)
  if not ok_jl then
    return
  end
  local count = type(current[1]) == "table" and #current[1] or 0
  if pos <= count then
    pcall(vim.cmd, "jump " .. pos)
  end
end

---@param entries project.JumpEntry[]
---@param opts? { stay_put?: boolean, file?: string, cursor?: { lnum: integer, col: integer } }
local function restore_jumplist_via_edits(entries, opts)
  opts = opts or {}
  pcall(vim.cmd.clearjumps)
  for _, jump in ipairs(entries) do
    local file = normalize_file(jump.file)
    if file and vim.fn.filereadable(file) == 1 then
      pcall(vim.cmd, "keepjumps edit " .. vim.fn.fnameescape(file))
      local line_count = vim.api.nvim_buf_line_count(0)
      local lnum = math.min(math.max(jump.lnum or 1, 1), line_count)
      pcall(vim.api.nvim_win_set_cursor, 0, { lnum, jump.col or 0 })
    end
  end

  if opts.stay_put then
    local file = normalize_file(opts.file)
    if file and vim.fn.filereadable(file) == 1 then
      pcall(vim.cmd, "keepjumps edit " .. vim.fn.fnameescape(file))
      M.apply_cursor(opts.cursor)
    end
  end
end

---@param jumplist project.JumplistState|nil
---@param opts? { stay_put?: boolean, file?: string, cursor?: { lnum: integer, col: integer } }
function M.restore_jumplist(jumplist, opts)
  opts = opts or {}
  if type(jumplist) ~= "table" then
    return
  end

  if jumplist.jumps_blob and load_jumps_blob(jumplist.jumps_blob) then
    if opts.stay_put then
      M.apply_cursor(opts.cursor)
    else
      restore_jump_position(jumplist.pos)
    end
    return
  end

  if jumplist.jumps_b64 and not jumplist.jumps_blob then
    local ok, blob = pcall(vim.base64.decode, jumplist.jumps_b64)
    if ok and blob and blob ~= "" then
      pcall(vim.cmd.clearjumps)
      local ok_load = pcall(vim.api.nvim_load_context, { jumps = blob })
      if ok_load then
        if opts.stay_put then
          M.apply_cursor(opts.cursor)
        else
          restore_jump_position(jumplist.pos)
        end
        return
      end
    end
  end

  local entries = jumplist.entries
  if not entries or #entries == 0 then
    return
  end

  restore_jumplist_via_edits(entries, opts)
  if not opts.stay_put then
    restore_jump_position(jumplist.pos)
  end
end

---@param root string|nil
function M.persist_jumplist(root)
  root = normalize_dir(root) or active_root
  if not root or lock or not M.is_managed_root(root) then
    return
  end

  local now = vim.uv.now()
  if now - last_jumplist_persist_ms < M.JUMPLIST_DEBOUNCE_MS then
    return
  end
  last_jumplist_persist_ms = now

  local captured = M.capture_jumplist()
  if not captured then
    return
  end

  local entry = M.get_project(root)
  entry.jumplist = captured
  M.set_project(root, entry)
end

-- ---------------------------------------------------------------------------
-- Buffer isolation
-- ---------------------------------------------------------------------------

---@param root string
---@param opts? { close_wins?: boolean }
function M.isolate_buffers(root, opts)
  opts = opts or {}
  root = normalize_dir(root)
  if not root then
    return
  end

  if opts.close_wins ~= false then
    local wins_to_close = {}
    for _, win in ipairs(vim.api.nvim_list_wins()) do
      if vim.api.nvim_win_is_valid(win) then
        local buf = vim.api.nvim_win_get_buf(win)
        local buf_root = M.buffer_root(buf)
        if buf_root and buf_root ~= root then
          wins_to_close[#wins_to_close + 1] = win
        end
      end
    end
    for _, win in ipairs(wins_to_close) do
      pcall(vim.api.nvim_win_close, win, false)
    end
  end

  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) then
      M.assign_buffer_project(buf)
      local buf_root = M.buffer_root(buf)
      if buf_root then
        vim.bo[buf].buflisted = buf_root == root
      end
    end
  end
end

-- ---------------------------------------------------------------------------
-- Project switch / state management
-- ---------------------------------------------------------------------------

---@return string|nil
function M.current_root()
  return normalize_dir(active_root) or normalize_dir(vim.fn.getcwd())
end

---@return { lnum: integer, col: integer }
function M.capture_cursor()
  local ok, cursor = pcall(vim.api.nvim_win_get_cursor, 0)
  if ok and type(cursor) == "table" then
    return { lnum = cursor[1], col = cursor[2] }
  end
  return { lnum = 1, col = 0 }
end

---@param cursor { lnum: integer, col: integer }|nil
function M.apply_cursor(cursor)
  if not cursor then
    return
  end
  local ok_count, line_count = pcall(vim.api.nvim_buf_line_count, 0)
  if not ok_count then
    return
  end
  local lnum = math.min(math.max(cursor.lnum, 1), line_count)
  pcall(vim.api.nvim_win_set_cursor, 0, { lnum, cursor.col or 0 })
end

---@param root string
---@return string|nil
function M.fallback_file(root)
  for _, name in ipairs(M.fallback_names) do
    local candidate = root .. "/" .. name
    if vim.fn.filereadable(candidate) == 1 then
      return candidate
    end
  end
  return nil
end

---@param entry project.Entry
---@param root string
---@return string|nil
function M.resolve_open_path(entry, root)
  local path = entry.last_file
  if path and vim.fn.filereadable(path) == 1 then
    return path
  end
  return M.fallback_file(root)
end

---@param root string
---@return string|nil
function M.get_last(root)
  root = normalize_dir(root)
  if not root then
    return nil
  end
  return M.resolve_open_path(M.get_project(root), root)
end

---@param root string
function M.save_state(root)
  root = normalize_dir(root)
  if not root or not M.is_managed_root(root) then
    return
  end

  local entry = M.get_project(root)
  local ok_buf, buf = pcall(vim.api.nvim_get_current_buf)
  if not ok_buf then
    return
  end

  if M.should_track(buf) then
    local file = normalize_file(vim.api.nvim_buf_get_name(buf))
    if file and file_in_root(file, root) then
      entry.last_file = file
      entry.cursor = M.capture_cursor()
    end
  end

  entry.jumplist = M.capture_jumplist() or entry.jumplist
  M.set_project(root, entry)
  last_jumplist_persist_ms = vim.uv.now()
end

---@param from_root string|nil
---@param to_root string|nil
---@param opts? { open_last?: boolean }
function M.on_root_change(from_root, to_root, opts)
  opts = opts or {}

  if lock then
    return
  end

  from_root = normalize_dir(from_root)
  to_root = normalize_dir(to_root)

  if from_root == to_root then
    return
  end

  if to_root and not M.is_managed_root(to_root) then
    vim.notify("Not a managed project: " .. to_root, vim.log.levels.WARN)
    return
  end

  lock = true

  local entry
  local ok, err = pcall(function()
    if from_root then
      M.save_state(from_root)
    end

    active_root = to_root

    if not to_root then
      return
    end

    local cwd = normalize_dir(vim.fn.getcwd())
    if cwd ~= to_root then
      vim.cmd.cd(vim.fn.fnameescape(to_root))
    end

    M.isolate_buffers(to_root, { close_wins = true })

    entry = M.get_project(to_root)

    if opts.open_last then
      local path = M.resolve_open_path(entry, to_root)
      if path then
        vim.cmd.edit(vim.fn.fnameescape(path))
        M.apply_cursor(entry.cursor)
      else
        vim.cmd.enew()
      end
    end
  end)

  lock = false

  if not ok then
    vim.notify("Project switch failed: " .. tostring(err), vim.log.levels.ERROR)
    return
  end

  if not to_root or not entry then
    return
  end

  vim.schedule(function()
    pcall(function()
      M.restore_jumplist(entry.jumplist, {
        stay_put = opts.open_last,
        file = entry.last_file,
        cursor = entry.cursor,
      })
    end)
  end)
end

---@param bufnr integer
function M.chdir_to_buffer(bufnr)
  M.assign_buffer_project(bufnr)
  local root = M.buffer_root(bufnr)
  if not root then
    return
  end

  local cwd = normalize_dir(vim.fn.getcwd())
  if root == cwd and root == active_root then
    M.isolate_buffers(root)
    return
  end

  M.on_root_change(active_root or cwd, root, { open_last = false })
end

---@param bufnr integer
function M.record(bufnr)
  if lock or not M.should_track(bufnr) then
    return
  end

  M.assign_buffer_project(bufnr)
  local root = M.buffer_root(bufnr)
  if not root then
    return
  end

  local path = normalize_file(vim.api.nvim_buf_get_name(bufnr))
  if not path then
    return
  end

  active_root = root

  local entry = M.get_project(root)
  local cursor = M.capture_cursor()

  if entry.last_file == path and entry.cursor and entry.cursor.lnum == cursor.lnum and entry.cursor.col == cursor.col then
    return
  end

  entry.last_file = path
  entry.cursor = cursor
  M.set_project(root, entry)
end

-- ---------------------------------------------------------------------------
-- Project discovery
-- ---------------------------------------------------------------------------

---@param git_path string
---@return string|nil
local function git_dir_to_root(git_path)
  git_path = git_path:gsub("/+$", "")
  if git_path == "" then
    return nil
  end
  if git_path:match("/%.git$") then
    return normalize_dir(vim.fn.fnamemodify(git_path, ":h"))
  end
  return normalize_dir(git_path)
end

---@param dir string
---@param max_depth integer
---@return string[]
local function collect_git_roots(dir, max_depth)
  dir = normalize_dir(dir)
  if not dir or vim.fn.isdirectory(dir) ~= 1 then
    return {}
  end

  local depth = max_depth + 1
  local quoted = vim.fn.shellescape(dir)
  local cmd

  if vim.fn.executable("fd") == 1 then
    cmd = string.format("fd -H -t d -d %d -g '.git' %s 2>/dev/null", depth, quoted)
  else
    cmd = string.format("find %s -name .git -type d -maxdepth %d 2>/dev/null", quoted, depth)
  end

  local ok, output = pcall(vim.fn.systemlist, cmd)
  if not ok or type(output) ~= "table" then
    return {}
  end

  local seen = {}
  local roots = {}
  for _, git_path in ipairs(output) do
    local root = git_dir_to_root(git_path)
    if root and not seen[root] then
      seen[root] = true
      roots[#roots + 1] = root
    end
  end
  return roots
end

---@return string[]
function M.list_projects()
  local projects_dir = get_projects_dir()
  if not projects_dir or vim.fn.isdirectory(projects_dir) ~= 1 then
    return {}
  end
  return collect_git_roots(projects_dir, config.max_depth or 3)
end

---@param root string
---@return string
function M.project_label(root)
  local projects_dir = get_projects_dir()
  if projects_dir and vim.startswith(root, projects_dir .. "/") then
    return root:sub(#projects_dir + 2)
  end
  return vim.fn.fnamemodify(root, ":t")
end

---@return project.PickerItem[]
function M.project_picker_items()
  local items = {}
  local s = get_state()
  for _, root in ipairs(M.list_projects()) do
    local entry = M.normalize_entry(s.projects[root])
    local last = M.resolve_open_path(entry, root)
    local hint = last and vim.fn.fnamemodify(last, ":~:.") or "(new)"
    items[#items + 1] = {
      project = root,
      text = string.format("%s — %s", M.project_label(root), hint),
    }
  end
  return items
end

-- ---------------------------------------------------------------------------
-- Per-project terminals
-- ---------------------------------------------------------------------------

local DEFAULT_TERMINAL_CONFIG = {
  position = "split",
  split = "botright",
  size = 15,
  float_width = 80,
  float_height = 15,
  border = "rounded",
}

---@return project.TerminalConfig
local function terminal_config()
  if not config.terminal then
    config.terminal = vim.deepcopy(DEFAULT_TERMINAL_CONFIG)
  end
  return config.terminal
end

---@return string
function M.terminal_config_line()
  local t = terminal_config()
  if t.position == "float" then
    return string.format("position=float %dx%d border=%s", t.float_width or 80, t.float_height or 15, t.border or "rounded")
  end
  return string.format("position=%s %s size=%d", t.position, t.split or "botright", t.size or 15)
end

---@param bufnr integer
---@return boolean
local function is_project_terminal_buf(bufnr)
  return vim.api.nvim_buf_is_valid(bufnr) and vim.bo[bufnr].buftype == "terminal" and vim.b[bufnr].project_terminal == true
end

---@param root string
---@return integer|nil
function M.terminal_find_buf(root)
  root = normalize_dir(root)
  if not root then
    return nil
  end

  local primary = primary_terminal[root]
  if primary and is_project_terminal_buf(primary) and normalize_dir(vim.b[primary].project_root) == root then
    return primary
  end

  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if is_project_terminal_buf(buf) and normalize_dir(vim.b[buf].project_root) == root then
      primary_terminal[root] = buf
      return buf
    end
  end

  primary_terminal[root] = nil
  return nil
end

---@param root string
---@param bufnr integer
local function register_terminal(root, bufnr)
  root = normalize_dir(root)
  if not root then
    return
  end
  vim.b[bufnr].project_root = root
  vim.b[bufnr].project_terminal = true
  primary_terminal[root] = bufnr
end

---@param bufnr integer
---@return integer|nil
local function terminal_visible_win(bufnr)
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_get_buf(win) == bufnr then
      return win
    end
  end
  return nil
end

---@param before integer[]
---@param prev_win integer
---@return integer
local function find_new_split_win(before, prev_win)
  local seen = {}
  for _, win in ipairs(before) do
    seen[win] = true
  end
  for _, win in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
    if not seen[win] then
      return win
    end
  end
  return prev_win
end

---@param t project.TerminalConfig
---@param prev_win integer
---@return integer
local function open_split_window(t, prev_win)
  local before = vim.api.nvim_tabpage_list_wins(0)
  local size = t.size or 15
  local mod = t.split or "botright"

  if t.position == "vsplit" then
    vim.cmd(string.format("keepalt %s %dvnew", mod, size))
    pcall(vim.api.nvim_set_current_win, prev_win)
    vim.cmd("wincmd l")
  else
    vim.cmd(string.format("keepalt %s %dnew", mod, size))
    pcall(vim.api.nvim_set_current_win, prev_win)
    vim.cmd("wincmd j")
  end

  local win = vim.api.nvim_get_current_win()
  if win == prev_win then
    win = find_new_split_win(before, prev_win)
    pcall(vim.api.nvim_set_current_win, win)
  end
  return win
end

---@param t project.TerminalConfig
---@return integer win
---@return integer bufnr
local function prepare_terminal_place(t)
  local prev_win = vim.api.nvim_get_current_win()

  if t.position == "tab" then
    vim.cmd("tabnew")
    local win = vim.api.nvim_get_current_win()
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_win_set_buf(win, buf)
    return win, buf
  end

  if t.position == "float" then
    local buf = vim.api.nvim_create_buf(false, true)
    local width = t.float_width or 80
    local height = t.float_height or 15
    local row = math.max(0, math.floor((vim.o.lines - height) / 2) - 1)
    local col = math.max(0, math.floor((vim.o.columns - width) / 2))
    local win = vim.api.nvim_open_win(buf, true, {
      relative = "editor",
      width = width,
      height = height,
      row = row,
      col = col,
      style = "minimal",
      border = t.border or "rounded",
    })
    return win, buf
  end

  local win = open_split_window(t, prev_win)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_win_set_buf(win, buf)
  pcall(vim.api.nvim_set_current_win, win)
  return win, buf
end

---@param bufnr integer
---@param t project.TerminalConfig
---@return integer
local function attach_terminal_window(bufnr, t)
  if t.position == "float" then
    local width = t.float_width or 80
    local height = t.float_height or 15
    local row = math.max(0, math.floor((vim.o.lines - height) / 2) - 1)
    local col = math.max(0, math.floor((vim.o.columns - width) / 2))
    local win = vim.api.nvim_open_win(bufnr, true, {
      relative = "editor",
      width = width,
      height = height,
      row = row,
      col = col,
      style = "minimal",
      border = t.border or "rounded",
    })
    vim.cmd.startinsert()
    return win
  end

  local prev_win = vim.api.nvim_get_current_win()
  local win
  if t.position == "tab" then
    vim.cmd("tabnew")
    win = vim.api.nvim_get_current_win()
  else
    win = open_split_window(t, prev_win)
  end

  vim.api.nvim_win_set_buf(win, bufnr)
  pcall(vim.api.nvim_set_current_win, win)
  vim.cmd.startinsert()
  return win
end

---@param bufnr integer
local function show_terminal_buffer(bufnr)
  local win = terminal_visible_win(bufnr)
  if win then
    pcall(vim.api.nvim_set_current_win, win)
    vim.cmd.startinsert()
    return
  end

  attach_terminal_window(bufnr, terminal_config())
end

---@param root string
---@return integer|nil
function M.terminal_create(root)
  root = normalize_dir(root)
  if not root or not M.is_managed_root(root) then
    vim.notify("No managed project for terminal", vim.log.levels.WARN)
    return nil
  end

  local was_lock = lock
  lock = true

  local t = terminal_config()
  local win, buf = prepare_terminal_place(t)
  register_terminal(root, buf)

  pcall(vim.api.nvim_set_current_win, win)
  local ok, job_id = pcall(vim.fn.termopen, vim.o.shell, { cwd = root })
  if not ok or job_id <= 0 then
    vim.notify("Failed to open terminal", vim.log.levels.ERROR)
    lock = was_lock
    return nil
  end

  pcall(vim.api.nvim_set_current_win, win)
  vim.cmd.startinsert()
  lock = was_lock
  return buf
end

---@return string|nil
function M.terminal_active_root()
  local root = M.current_root()
  if root and M.is_managed_root(root) then
    return root
  end
  vim.notify("No managed project for terminal", vim.log.levels.WARN)
  return nil
end

function M.terminal_toggle()
  local root = M.terminal_active_root()
  if not root then
    return
  end

  local buf = M.terminal_find_buf(root)
  if buf and vim.api.nvim_get_current_buf() == buf and terminal_visible_win(buf) then
    if #vim.api.nvim_tabpage_list_wins(0) > 1 then
      pcall(vim.api.nvim_win_close, 0, false)
    else
      local path = M.get_last(root)
      if path then
        vim.cmd.edit(vim.fn.fnameescape(path))
      else
        vim.cmd.enew()
      end
    end
    return
  end

  if buf then
    show_terminal_buffer(buf)
    return
  end

  M.terminal_create(root)
end

function M.terminal_new()
  local root = M.terminal_active_root()
  if not root then
    return
  end
  M.terminal_create(root)
end

---@param args string
---@return boolean
function M.terminal_configure(args)
  args = vim.trim(args or "")
  if args == "" or args == "show" then
    vim.notify("Project terminal: " .. M.terminal_config_line(), vim.log.levels.INFO)
    return false
  end

  local t = terminal_config()
  local positional = {}
  local numbers = {}

  for part in args:gmatch("%S+") do
    local key, value = part:match("^([%w_]+)=(.+)$")
    if key then
      key = key:lower()
      if key == "position" then
        t.position = value:lower()
      elseif key == "split" or key == "modifier" then
        t.split = value
      elseif key == "size" then
        t.size = tonumber(value) or t.size
      elseif key == "height" then
        local n = tonumber(value)
        if n then
          t.size = n
          t.float_height = n
        end
      elseif key == "width" then
        local n = tonumber(value)
        if n then
          t.float_width = n
        end
      elseif key == "float_height" then
        t.float_height = tonumber(value) or t.float_height
      elseif key == "border" then
        t.border = value
      end
    else
      part = part:lower()
      positional[#positional + 1] = part
      local n = tonumber(part)
      if n then
        numbers[#numbers + 1] = n
      end
    end
  end

  for _, part in ipairs(positional) do
    if part == "split" or part == "vsplit" or part == "tab" or part == "float" then
      t.position = part
    elseif part == "botright" or part == "topleft" or part == "vertical" or part == "horizontal" then
      t.split = part
    end
  end

  if #numbers >= 2 and t.position == "float" then
    t.float_width = numbers[1]
    t.float_height = numbers[2]
  elseif #numbers >= 1 then
    t.size = numbers[1]
  end

  vim.notify("Project terminal: " .. M.terminal_config_line(), vim.log.levels.INFO)
  return true
end

local TERM_ACTIONS = { new = true, show = true, toggle = true }

---@param opts { args: string, bang: boolean }
function M.terminal_cmd(opts)
  local action = opts.bang and "new" or "toggle"
  local layout_parts = {}

  for part in (opts.args or ""):gmatch("%S+") do
    local word = part:lower()
    if TERM_ACTIONS[word] then
      action = word
    else
      layout_parts[#layout_parts + 1] = part
    end
  end

  if #layout_parts > 0 then
    M.terminal_configure(table.concat(layout_parts, " "))
  end

  if action == "show" then
    if #layout_parts == 0 then
      M.terminal_configure("show")
    end
    return
  end

  if action == "new" then
    M.terminal_new()
  else
    M.terminal_toggle()
  end
end

local TERM_CMD_KEYS = { "position=", "size=", "split=", "width=", "height=", "border=" }
local TERM_CMD_WORDS = { "toggle", "new", "show", "split", "vsplit", "tab", "float", "botright", "topleft" }

---@param _ string
---@param line string
---@return string[]
local function term_cmd_complete(_, line)
  local parts = vim.split(vim.trim(line), "%s+")
  local last = parts[#parts] or ""
  if last:find("=") then
    return vim.tbl_filter(function(c)
      return vim.startswith(c, last)
    end, TERM_CMD_KEYS)
  end
  return vim.tbl_filter(function(c)
    return vim.startswith(c, last)
  end, TERM_CMD_WORDS)
end

-- ---------------------------------------------------------------------------
-- Startup restoration
-- ---------------------------------------------------------------------------

function M.restore_startup()
  local root = active_root

  if not root then
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      local buf_root = M.buffer_root(buf)
      if buf_root then
        root = buf_root
        break
      end
    end
  end

  if not root then
    root = normalize_dir(vim.fn.getcwd())
  end

  if not root or not M.is_managed_root(root) then
    return
  end

  local open_last = vim.fn.argc(-1) == 0
  M.on_root_change(nil, root, { open_last = open_last })
end

function M.switch()
  local items = M.project_picker_items()
  if #items == 0 then
    vim.notify("No projects found under " .. (config.projects_dir or "~/dev"), vim.log.levels.WARN)
    return
  end

  vim.ui.select(items, {
    prompt = "Project",
    format_item = function(item)
      return item.text
    end,
  }, function(choice)
    if not choice or not choice.project then
      return
    end
    vim.schedule(function()
      M.on_root_change(active_root, choice.project, { open_last = true })
    end)
  end)
end

-- ---------------------------------------------------------------------------
-- Setup
-- ---------------------------------------------------------------------------

---@param opts? project.Config
function M.setup(opts)
  opts = opts or {}

  config = {
    projects_dir = opts.projects_dir or (vim.fn.expand("~/dev") --[[@as string]]),
    max_depth = opts.max_depth or 3,
    store_path = opts.store_path,
    terminal = vim.tbl_deep_extend("force", vim.deepcopy(DEFAULT_TERMINAL_CONFIG), opts.terminal or {}),
  }

  M.load_state()
  if state_dirty then
    M.persist_state(true)
  end
  start_persist_timer()

  active_root = normalize_dir(vim.fn.getcwd())
  if active_root and M.is_managed_root(active_root) then
    M.isolate_buffers(active_root)
  else
    active_root = nil
  end

  local group = vim.api.nvim_create_augroup("project", { clear = true })

  vim.api.nvim_create_autocmd({ "BufNew", "TermOpen" }, {
    group = group,
    callback = function(args)
      if not lock then
        M.assign_buffer_project(args.buf)
      end
    end,
  })

  vim.api.nvim_create_autocmd("BufDelete", {
    group = group,
    callback = function(args)
      for root, buf in pairs(primary_terminal) do
        if buf == args.buf then
          primary_terminal[root] = nil
        end
      end
    end,
  })

  vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
    group = group,
    callback = function(args)
      if lock then
        return
      end
      if args.event == "BufEnter" then
        M.chdir_to_buffer(args.buf)
        M.record(args.buf)
      end
      if active_root then
        M.isolate_buffers(active_root)
      end
    end,
  })

  vim.api.nvim_create_autocmd("VimLeavePre", {
    group = group,
    callback = function()
      if active_root then
        M.save_state(active_root)
      end
      M.persist_state(true)
    end,
  })

  vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
    group = group,
    callback = function()
      if active_root then
        M.persist_jumplist(active_root)
      end
    end,
  })

  vim.api.nvim_create_autocmd("VimEnter", {
    group = group,
    once = true,
    callback = function()
      vim.schedule(function()
        M.restore_startup()
      end)
    end,
  })

  local term_cmd_opts = {
    nargs = "*",
    bang = true,
    complete = term_cmd_complete,
  }

  vim.api.nvim_create_user_command("ProjectSwitch", M.switch, { desc = "Switch project" })
  vim.api.nvim_create_user_command("P", M.switch, { desc = "Alias → ProjectSwitch" })

  vim.api.nvim_create_user_command("ProjectSave", function()
    local root = M.current_root()
    if not root then
      vim.notify("No active project", vim.log.levels.WARN)
      return
    end
    M.save_state(root)
    M.persist_state(true)
    vim.notify(string.format("Saved project state: %s", M.project_label(root)))
  end, { desc = "Save current project state" })

  vim.api.nvim_create_user_command("ProjectStatus", function()
    local root = M.current_root()
    local path = store_path()
    if root then
      vim.notify(string.format("Project: %s\nState: %s\nTerminal: %s", root, path, M.terminal_config_line()))
    else
      vim.notify(string.format("No project root\nState: %s\nTerminal: %s", path, M.terminal_config_line()))
    end
  end, { desc = "Show active project and state file" })

  local function project_term_cmd(cmd)
    M.terminal_cmd({ args = cmd.args, bang = cmd.bang })
  end

  vim.api.nvim_create_user_command("ProjectTerm", project_term_cmd, vim.tbl_extend("force", {
    desc = "Project terminal: toggle (default), ! or new, show, layout opts",
  }, term_cmd_opts))

  vim.api.nvim_create_user_command("Pt", project_term_cmd, vim.tbl_extend("force", {
    desc = "Alias → ProjectTerm",
  }, term_cmd_opts))
end

return M
