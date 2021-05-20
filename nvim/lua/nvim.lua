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
    if type(f) == 'function' then
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
    if type(f) == 'function' then
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
      print(cmd)
    end
    vim.api.nvim_set_keymap(mode, keyseq, cmd, {noremap = true})
  end
end

local function nvim_colorscheme(name)
  vim.cmd([[ colorscheme ]] .. name)
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
