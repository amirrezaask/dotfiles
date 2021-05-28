-- Force reload the module
function R(mod)
  package.loaded[mod] = nil
  return require(mod)
end

-- is the package loaded ?
function L(mod)
  return package.loaded[mod] ~= nil
end

-- Printer
function P(obj)
  print(vim.inspect(obj))
end

local function tohex(s)
  local R = {}
  for i = 1, #s do
    R[#R+1] = string.format("%02X", s:byte(i))
  end
  return table.concat(R)
end

__AUTOCMD_REGISTRY = {}
--@param opts[1] event
--@param opts[2] filter
--@param opts[3] function or expression
local function nvim_autocmd(opts)
 local function get_expression(f)
    if type(f) == 'string' then return f end
    if type(f) == 'function' or type(f) == 'table' then
      __AUTOCMD_REGISTRY[tohex(opts[1] .. opts[2])] = function()
        f()
      end
      return string.format('lua __AUTOCMD_REGISTRY["%s"]()', tohex(opts[1]..opts[2]))
    end
  end
  vim.cmd(string.format('autocmd %s %s %s', opts[1], opts[2], get_expression(opts[3])))
end

local function nvim_highlight(name, guifg, guibg)
  local t = { 'hi', name }
  if guifg then
    table.insert(t, string.format('guifg=%s', guifg))
  end
  if guibg then
    table.insert(t, string.format('guibg=%s', guibg))
  end
  vim.cmd(table.concat(t, ' '))
end

local function nvim_augroup(tbl)
  for g, _ in pairs(tbl) do
    vim.cmd('augroup ' .. g)
    vim.cmd('autocmd!')
    vim.autocmd(tbl[g])
    vim.cmd('augroup END')
  end
end

__MAP_REGISTRY = {}
local function nvim_map(keys)
  local function get_char(s, n)
    return s:sub(n, n)
  end
  local function keymap(s)
    return s:sub(3, -1)
  end
  local function parse_key(key)
    if #vim.split(key, ' ') > 1 then
      local mode = get_char(key, 1)
      local keyseq = vim.api.nvim_replace_termcodes(keymap(key), true, true, true)
      return mode, keyseq
    end
    return '', key
  end

  local function get_key_cmd(k, f)
    if type(f) == 'string' then return f end
    if type(f) == 'function' or type(f) == 'table' then
      __MAP_REGISTRY[tohex(k)] = function()
        f()
      end
      return string.format('<cmd>lua __MAP_REGISTRY["%s"]()<cr>', tohex(k))
    end
  end
  for k, f in pairs(keys) do
    local mode, keyseq = parse_key(k)
    local cmd = get_key_cmd(k, f)
    if cmd == nil then
      print(k)
      print(vim.inspect(f))
      print(cmd)
    end
    vim.api.nvim_set_keymap(mode, keyseq, cmd, {noremap = true})
  end
end

local function make_mapper(mode)
  return function(keymaps)
    local new_keymaps = {}
    for k, fn in pairs(keymaps) do
      new_keymaps[string.format('%s %s', mode, k)] = fn
    end
    nvim_map(new_keymaps)
  end
end

__COMMAND_REGISTRY = {}
local function nvim_command(name, expr, args)
  if type(expr) == 'function' then
    local fn = expr
    __COMMAND_REGISTRY[name] = function()
      fn()
    end
    expr = string.format('lua __COMMAND_REGISTRY["%s"]()<CR>', name)
  end
  if not args then
    vim.cmd(string.format('command! %s %s', name, expr))
  end
  if args then
    vim.cmd(string.format('command! -nargs=%s %s %s', args, name, expr))
  end
end

local o_vim_highlight = vim.highlight
vim.highlight = setmetatable({}, {
  __index = o_vim_highlight,
  __call = function(_, ...)
    nvim_highlight(...)
  end
})
vim.autocmd = nvim_autocmd
vim.augroup = nvim_augroup

vim.map = nvim_map
vim.nmap = make_mapper('n')
vim.vmap = make_mapper('v')
vim.tmap = make_mapper('t')
vim.imap = make_mapper('i')

vim.command = setmetatable({}, {
  __call = function(_, ...) nvim_command(...) end,
  __index = function(_, name)
    return function(...)
        local args = {...}
        local cmd = {name}
        for _, a in ipairs(args) do
          table.insert(cmd, a)
        end
        vim.cmd(table.concat(cmd, ' '))
      end
  end
})
vim.c = vim.command

-- Took from tjdevries/astronauta.nvim
-- Load ftplugin/*.lua and after/ftplugin/*.lua for * FileType 
vim.autocmd {
  "Filetype",
  "*",
  function()
    local function load_file(f)
      local env = setmetatable({
          print = vim.schedule_wrap(print)
          },
          {
            __index = _G,
            __newindex = _G
          })
      local floader = loadfile(f)
      pcall(setfenv(floader, env))
    end

    local function find_ftplugins_for (filetype)
      local patterns = {
        string.format("ftplugin/%s.lua", filetype),
      }

      local result = {}
      for _, pat in ipairs(patterns) do
        vim.list_extend(result, vim.api.nvim_get_runtime_file(pat, true))
      end

      return result
    end

    local ftplugins = find_ftplugins_for(vim.fn.expand('<amatch>'))
    for _, file in ipairs(ftplugins) do
      load_file(file)
    end
  end
}
