local function mode()
  local m = vim.fn.mode()
  if m == 'n' then
    return '%#Function# Normal %*'
  elseif m == 'v' or m == 'V' then
    return '%#StatusLine# Visual %*'
  elseif m == 'i' then
    return '%#StatusLine# Insert %*'
  elseif m == 'ic' or m == 'ix' then
    return ' IComplete '
  elseif m == 'c' then
    return '%#Constant# Command %*'
  elseif m == 't' then
    return '%#Constant# Terminal %*'
  else
    return m
  end
end

-- local filename = '%f%<'

local shorten_path = (function()
  if jit then
    local ffi = require('ffi')
    ffi.cdef [[
    typedef unsigned char char_u;
    char_u *shorten_dir(char_u *str);
    ]]

    return function(filepath)
      if not filepath then
        return filepath
      end

      local c_str = ffi.new("char[?]", #filepath + 1)
      ffi.copy(c_str, filepath)
      return ffi.string(ffi.C.shorten_dir(c_str))
    end
  else
    return function(filepath)
      return filepath
    end
  end
end)()

local function filename(shorten)
  local name = vim.api.nvim_buf_get_name(0)
  if shorten then
    return shorten_path(name)
  else
    return name
  end
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

local line_col = '[ %l:%c %P ]'

local filetype = '%y'

local sep = '%='

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
  return '' 
end

local __BRANCH = ''

function StatusLineUpdateGitBranch()
  __BRANCH = vim.fn['fugitive#head']()
  if __BRANCH ~= '' then
   __BRANCH = require('nvim-web-devicons').get_icon('git', 'git', {default=true}) .. ' ' .. __BRANCH
  end
end

-- Only update statusline on BufEnter
vim.cmd [[ autocmd BufEnter * lua StatusLineUpdateGitBranch() ]]

function Statusline()
  local statusline = ''
  statusline = statusline .. mode()
  statusline = statusline .. ' ' .. __BRANCH
  statusline = statusline .. sep
  statusline = statusline .. ' ' .. get_icon(vim.api.nvim_buf_get_name(0)) .. ' ' .. filename(true) .. '%m'
  statusline = statusline .. sep
  statusline = statusline .. ' ' .. line_col
  statusline = statusline .. ' ' .. filetype
  statusline = statusline .. ' ' .. lsp_info()
  return statusline
end

vim.o.statusline = '%!v:lua.Statusline()'
