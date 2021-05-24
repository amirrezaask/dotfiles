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

vim.opt = setmetatable({}, {
  __index = function(_, k)
    return vim.o[k]
  end,
  __newindex = function(_, k, v)
    if type(v) == "boolean" then
      if v then
        vim.cmd(string.format([[ set %s ]], k))
      else
        vim.cmd(string.format([[ set no%s ]], k))
      end
      return
    elseif type(v) == "table" and vim.tbl_islist(v) then
      vim.cmd(string.format([[ set %s=%s ]], k, table.concat(v, ',')))
    else
      vim.cmd(string.format([[ set %s=%s ]], k, v))
    end
  end
})

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
    return setmetatable({}, {
      __call = function(_, ...)
        local args = {...}
        local cmd = {name}
        for _, a in ipairs(args) do
          table.insert(cmd, a)
        end
        vim.cmd(table.concat(cmd, ' '))
      end
    })
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

local function eval_line()
  vim.cmd [[ w ]]
  local lnum = vim.api.nvim_win_get_cursor(0)[1]
  local line = vim.api.nvim_buf_get_lines(0, lnum - 1, lnum, false)[1]
  local function wrap(l)
    return string.format('return(vim.inspect(%s))', l)
  end
  local result = load(wrap(line))()
  if result ~= nil then
    local inlay = require('amirrezaask.inlayhints').for_buf(vim.api.nvim_get_current_buf())
    inlay:set({
      lnum = lnum - 1,
      line = result
    })
    vim.c.write()
    vim.c.edit()
  end
end

local function eval_file()
  local filetype = vim.api.nvim_buf_get_option(0, 'filetype')
  if filetype == 'lua' then
    vim.cmd(string.format('luafile %s', vim.api.nvim_buf_get_name(0)))
  elseif filetype == 'vim' then
    vim.cmd(string.format('source %s', vim.api.nvim_buf_get_name(0)))
  end
end

vim.map {
  [',x'] = eval_line,
  ['<M-x>'] = eval_file
}

function LoadLuaPlugins()
  for _, file in ipairs(vim.api.nvim_get_runtime_file('lua/plugin/**/*.lua', true)) do
    local ok, msg = pcall(loadfile(file))
    if not ok then
      print("Failed to load: ", file)
      print("\t", msg)
    end
  end
end

