local has_lspstatus, lspstatus = pcall(require, 'lsp-status')
local default_icons = {
    error = '😡',
    warning = '😳',
    info = '🛈',
    hint = '😅',
    ok = '🆗',
    ['function'] = '',
}
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
parts.pipe = '|'
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
  return function()
    local result = item
    if type(item) == 'function' then
      result = result()
    end
    if result == nil or result == '' then
      return ''
    end
    return '[' .. result .. ']'
  end
end

function wrappers.parens(item)
  return function()
    local result = item
    if type(item) == 'function' then
      result = result()
    end
    if result == nil or result == '' then
      return ''
    end
    return '(' .. result .. ')'
  end
end

function wrappers.curly_brackets(item)
  return function()
    local result = item
    if type(item) == 'function' then
      result = result()
    end
    if result == nil or result == '' then
      return ''
    end
    return '{' .. result .. '}'
  end
end

__STATUSLINE = nil

local function make_statusline(elements, opts)
  opts = opts or {}
  __STATUSLINE = function()
    local _parts = {}
    for _, e in ipairs(elements) do
      if type(e) == 'function' then
        table.insert(_parts, e())
      elseif type(e) == 'table' then
        table.insert(_parts, table.concat(e, ''))
      else
        table.insert(_parts, e)
      end
    end
    return table.concat(_parts, opts.delimiter or '')
  end
end

function parts.lsp_progress()
  if not has_lspstatus then return '' end
  return lspstatus.status_progress()
end

function parts.lsp_current_function(symbol)
  symbol = symbol or default_icons['function']
  return function()
    local ok, current_function = pcall(vim.api.nvim_buf_get_var,0, 'lsp_current_function')
    if ok and current_function ~= '' then
      if symbol == '' then
        return current_function
      else
        return symbol .. ' ' .. current_function
      end
    else
      return ''
    end
  end
end

function parts.lsp_diagnostics(icons)
  icons = icons or {}
  return function()
    local diag = lspstatus.diagnostics()
    local output = {}
    if diag.errors ~= 0 then
      table.insert(output, string.format("%s %s", icons.error or default_icons.error, diag.errors))
    end
    if diag.warnings ~= 0 then
      table.insert(output, string.format("%s %s", icons.warning or default_icons.warning, diag.warnings))
    end
    if diag.hints ~= 0 then
      table.insert(output, string.format("%s %s", icons.hint or default_icons.hint, diag.hints))
    end
    if diag.info ~= 0 then
      table.insert(output, string.format("%s %s", icons.info or default_icons.info, diag.info))
    end
    if #output < 1 then return icons.ok or default_icons.ok end
    return table.concat(output, ' ')
  end
end

vim.opt.statusline = '%!v:lua.__STATUSLINE()'

return {
  make = make_statusline,
  parts = parts,
  wrappers = wrappers,
}

