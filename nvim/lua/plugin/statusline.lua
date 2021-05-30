local function mode()
  local m = vim.api.nvim_get_mode().mode
  if m == 'n' then
    return '%#NormalMode# Normal %*'
  elseif m == 'v' or m == 'V' then
    return '%#VisualMode# Visual %*'
  elseif m == '' then
    return '%#VisualMode# VisualBlock %*'
  elseif m == 'i' then
    return '%#InsertMode# Insert %*'
  elseif m == 'ic' or m == 'ix' then
    return '%#InsertMode# IComplete %*'
  elseif m == 'c' then
    return '%#CommandMode# Command %*'
  elseif m == 't' then
    return '%#TerminalMode# Terminal %*'
  else
    return m
  end
end
local has_lspstatus, lspstatus = pcall(require, 'lsp-status')
if has_lspstatus then
  lspstatus.config({
    indicator_errors = 'E:',
    indicator_warnings = 'W:',
    indicator_info = 'I: ',
    indicator_hint = 'H:',
    indicator_ok = 'OK',
  })
end

local modified = '%m'
local readonly = '%r'
local space = ' '
local filename = '%f'
local filename_shorten = "%{pathshorten(expand('%:f'))}"
local pipe = ' | '
local line_col = '[ %l:%c %%%p ]'
local line = '%l'
local col = '%c'
local percentage_of_file = '%%%p'

local filetype = '%y'

local seperator = '%='

local function get_icon()
  local file = vim.api.nvim_buf_get_name(0)
  local has_icons, _ = pcall(require, 'nvim-web-devicons')
  if not has_icons then
    print('for having icon in drawer install `nvim-web-devicons`')
    return false
  end
  local icon, _ = require('nvim-web-devicons').get_icon(file, string.match(file, '%a+$'), { default = true })
  if icon ~= '' then
    return icon
  end
  return '' 
end

local function git_branch_icon()
  local has_icons, _ = pcall(require, 'nvim-web-devicons')
  if not has_icons then
    print('for having icon in drawer install `nvim-web-devicons`')
    return false
  end
  local icon, _ = require('nvim-web-devicons').get_icon('git', 'git', { default = true })
  if icon ~= '' then
    return icon
  end
  return ''
end

local function git_branch()
  local __BRANCH
  local success
  success, __BRANCH = pcall(vim.fn['fugitive#head'])
  if success and __BRANCH ~= '' then
    return __BRANCH
   -- __BRANCH = require('nvim-web-devicons').get_icon('git', 'git', {default=true}) .. ' ' .. __BRANCH
  end
  return __BRANCH
end

local function with_brackets(item)
  return '[' .. item .. ']'
end

local colon = ':'

__STATUSLINE = nil

local function make_statusline(elements, opts)
  opts = opts or {}
  __STATUSLINE = function()
    local parts = {}
    for _, e in ipairs(elements) do
      if type(e) == 'function' then
        table.insert(parts, e())
      else
        table.insert(parts, e)
      end
    end
    return table.concat(parts, opts.delimiter or '')
  end
end

local function lsp_status()
  if not has_lspstatus then return '' end
  return with_brackets(lspstatus.status())
end

make_statusline {
  mode,
  space,
  git_branch_icon, space, git_branch,
  seperator, space,
  get_icon,
  space,
  filename_shorten,
  modified,
  seperator,
  space,
  line_col,
  filetype,
  lsp_status,
}

-- make_statusline ({
--   table.concat({space, line, colon, col}, ''),
--   table.concat({readonly, modified, filename }, ''),
--   filetype,
--   git_branch,
--   lsp_info
-- }, {delimiter = ' | '})

vim.opt.statusline = '%!v:lua.__STATUSLINE()'
