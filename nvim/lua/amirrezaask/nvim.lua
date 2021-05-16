local M = {}
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
function M.autocmd(opts)
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

function M.highlight(name, guifg, guibg)
  local t = { 'hi', name }
  if guifg then
    table.insert(t, string.format('guifg=%s', guifg))
  end
  if guibg then
    table.insert(t, string.format('guibg=%s', guibg))
  end
  vim.cmd(table.concat(t, ' '))
end

function M.augroup(tbl)
  for g, _ in pairs(tbl) do
    vim.cmd('augroup ' .. g)
    M.autocmd(tbl[g])
    vim.cmd('augroup END')
  end
end

function M.with_options(tbl)
  for n, v in pairs(tbl) do
    if type(v) == "boolean" then
      if v then
        vim.cmd(string.format([[ set %s]], n))
      else
        vim.cmd(string.format([[ set no%s]], n))
      end
    else
      vim.cmd(string.format([[ set %s=%s ]], n, v))
    end
  end
end

__MAP_REGISTRY = {}
function M.map(keys)
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

function M.colorscheme(name)
  vim.cmd([[ colorscheme ]] .. name)
end

__COMMAND_REGISTRY = {}
function M.command(name, expr, args)
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
return M
