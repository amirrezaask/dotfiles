local spawn = require('spawn')

local function mode()
  local m = vim.fn.mode()
  if m == 'n' then
    return '%#Function#[Normal]%*'
  elseif m == 'v' or m == 'V' then
    return '%#StatusLine#[Visual]%*'
  elseif m == 'i' then
    return '%#StatusLine#[Insert]%*'
  elseif m == 'ic' or m == 'ix' then
    return '[IComplete]'
  elseif m == 'c' then
    return '%#Constant#[Command]%*'
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
  return '[ %l:%c ]'
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

local __BRANCH = ''

function StatusLineUpdateGitBranch()
  local branch = spawn{
    command = 'git',
    args = {'branch', '--show-current'},
    sync = {timeout=1000}
  }
  if branch ~= nil and branch ~= '' and #branch > 0 then
    local has_icons, _ = pcall(require, 'nvim-web-devicons')
    if not has_icons then
      __BRANCH = branch[1]
    end
    local icon, _ = require('nvim-web-devicons').get_icon('', 'git')
    __BRANCH = icon .. ' ' .. branch[1]
  end
  return ''
end

-- Only update statusline on BufEnter
vim.cmd [[ autocmd BufEnter * lua StatusLineUpdateGitBranch() ]]

function Statusline()
  local statusline = ''
  statusline = statusline .. mode()
  statusline = statusline .. ' ' .. __BRANCH
  statusline = statusline .. sep()
  statusline = statusline .. ' ' .. get_icon(vim.api.nvim_buf_get_name(0)) .. ' ' .. filename()
  statusline = statusline .. sep()
  statusline = statusline .. ' ' .. line_col()
  statusline = statusline .. ' ' .. filetype()
  statusline = statusline .. ' ' .. lsp_info()
  return statusline
end

vim.o.statusline = '%!v:lua.Statusline()'
