local has_lspstatus, lspstatus = pcall(require, 'lsp-status')

local parts = {}
local wrappers = {}

function parts.mode()
  local m = vim.api.nvim_get_mode().mode
  if m == 'n' then
    return '%#NormalMode# Normal %*'
  elseif m == 'v' or m == 'V' then
    return '%#VisualMode# Visual %*'
  elseif m == '' then
    return '%#VisualBlockMode# VisualBlock %*'
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

parts.modified = '%m'
parts.readonly = '%r'
parts.space = ' '
parts.filename = '%f'
parts.filename_shorten = "%{pathshorten(expand('%:f'))}"
parts.pipe = ' | '
parts.line_col = '[ %l:%c %%%p ]'
parts.line = '%l'
parts.col = '%c'
parts.percentage_of_file = '%%%p'
parts.filetype = '%y'
parts.seperator = '%='
parts.colon = ':'

parts.icons = {
  file = function()
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
  end,
  git = function()
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

}

function parts.git_branch()
  local __BRANCH
  local success
  success, __BRANCH = pcall(vim.fn['fugitive#head'])
  if success and __BRANCH ~= '' then
    return __BRANCH
  end
  return __BRANCH
end

function wrappers.square_brackets(item)
  if type(item) == 'function' then item = item() end
  if item == nil or item == '' then return '' end
  return '[' .. item .. ']'
end

function wrappers.parens(item)
  if type(item) == 'function' then item = item() end
  if item == nil or item == '' then return '' end
  return '(' .. item .. ')'
end

function wrappers.curly_brackets(item)
  if type(item) == 'function' then item = item() end
  if item == nil or item == '' then return '' end
  return '{' .. item .. '}'
end

__STATUSLINE = nil

local function make_statusline(elements, opts)
  opts = opts or {}
  __STATUSLINE = function()
    local _parts = {}
    for _, e in ipairs(elements) do
      if type(e) == 'function' then
        table.insert(_parts, e())
      else
        table.insert(_parts, e)
      end
    end
    return table.concat(_parts, opts.delimiter or '')
  end
end

function parts.lsp_status()
  if not has_lspstatus then return '' end
  return lspstatus.status()
end

vim.autocmd {
  "CursorMoved,CursorMovedI",
  "*",
  "lua require('lsp-status').update_current_function()"
}

vim.opt.statusline = '%!v:lua.__STATUSLINE()'

return {
  make = make_statusline,
  parts = parts,
  wrappers = wrappers,
}

