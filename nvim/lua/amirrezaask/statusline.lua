-- Goals: Line:Col | FileName | Branch | LSP Diagnostic

local function mode()
  local m = vim.fn.mode()
  if m == 'n' then
    return '[NORMAL]'
  elseif m == 'v' or m == 'V' then
    return '[VISUAL]'
  elseif m == 'i' or m == 'ic' then
    return '[INSERT]'
  elseif m == 'c' then
    return '[COMMAND]'
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

  return string.format('LSP: H %d W %d E %d', hints, warnings, errors)
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
local function git_branch()
  local handler = io.popen('git branch --show-current')
  local branch = handler:read('*all')
  return '[' .. vim.split(branch, '\n')[1] .. ']'
end
function Statusline()
  local statusline = ''
  statusline = statusline .. mode()
  statusline = statusline .. ' ' .. line_col()
  statusline = statusline .. sep()
  statusline = statusline .. git_branch()
  statusline = statusline .. ' ' .. filename()
  statusline = statusline .. ' ' .. filetype()
  statusline = statusline .. sep()
  statusline = statusline .. lsp_info()
  return statusline
end

vim.api.nvim_set_option('statusline', '%!v:lua.Statusline()')
