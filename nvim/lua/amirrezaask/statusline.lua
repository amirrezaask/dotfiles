local function mode()
  local m = vim.fn.mode()
  if m == 'n' then
    return '[Normal]'
  elseif m == 'v' or m == 'V' then
    return '[Visual]'
  elseif m == 'i' or m == 'ic' then
    return '[Insert]'
  elseif m == 'c' then
    return '[Command]'
  else
    return m
  end
end
local function filename()
  return '%f%<'
end
local function lsp_info()
  local warnings = vim.lsp.diagnostic.get_count(0, 'Warning')
  local errors = vim.lsp.diagnostic.get_count(0, 'Error')
  local hints = vim.lsp.diagnostic.get_count(0, 'Hint')
  local infos = vim.lsp.diagnostic.get_count(0, 'Information')
  local output = ''
  if hints ~= 0 then
    output = output .. ' H: ' .. hints
  end
  if errors ~= 0 then
    output = output .. ' E: ' .. errors
  end
  if warnings ~= 0 then
    output = output .. ' W: ' .. warnings
  end
  if infos ~= 0 then
    output = output .. ' I: ' .. infos
  end
  if output == '' then
    return ''
  end
  output = output .. ' '
  return '[' .. output .. ']'
end

local function line_col()
  return '%l:%c'
end

local function filetype()
  return '%y'
end
local function sep()
  return '%='
end
local function get_icon(file)
  local has_icons, _ = pcall(require, 'nvim-web-devicons')
  if not has_icons then
    print('for having icon in drawer install `nvim-web-devicons`')
    return false
  end
  local icon, _ = require('nvim-web-devicons').get_icon(file, string.match(file, '%a+$'), { default = true })
  if icon ~= '' then
    return icon
  end
  return false
end

local function git_branch()
  local handler = io.popen('git branch --show-current 2>/dev/null')
  local branch = handler:read('*all')
  branch = vim.split(branch, '\n')[1]
  if branch ~= '' then
    return '[' .. branch .. ']'
  else
    return ''
  end
end
function Statusline()
  local statusline = ''
  statusline = statusline .. mode()
  statusline = statusline .. ' ' .. line_col()
  statusline = statusline .. sep()
  statusline = statusline .. ' ' .. get_icon(vim.api.nvim_buf_get_name(0)) .. ' ' .. filename()
  statusline = statusline .. sep()
  statusline = statusline .. ' ' .. git_branch()
  statusline = statusline .. ' ' .. filetype()
  statusline = statusline .. ' ' .. lsp_info()
  return statusline
end
vim.o.statusline = '%!v:lua.Statusline()'
