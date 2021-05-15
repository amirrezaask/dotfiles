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
local modified = '%m'
local readonly = '%r'
local space = ' '
local simple_filename = '%f'

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

local function filename(opts)
  opts = opts or {}
  if opts.shorten == nil then opts.shorten = true end
  opts.from_root = false
  local name
  if opts.from_root then
    name = vim.api.nvim_buf_get_name(0)
  else
    name = vim.fn.expand('%%') 
  end
  if opts.shorten then
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

local line_col = '[ %l:%c %%%p ]'
local line = '%l'
local col = '%c'
local percentage_of_file = '%P'

local filetype = '%y'

local sep = '%='

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

local function wrap(item)
  return '[' .. item .. ']'
end

function ExpresslineLike()
  return mode() .. ' ' .. wrap(git_branch()) .. sep .. ' ' .. get_icon() .. ' ' ..
    filename({shorten=false}) .. '%m' .. sep .. ' ' .. line_col .. filetype .. lsp_info()
end

function Compact()
  return modified .. readonly .. ' ' .. line .. ':' .. col .. ' ' ..simple_filename .. sep .. space .. percentage_of_file .. space .. filetype .. space .. wrap(git_branch())
end

vim.o.statusline = '%!v:lua.ExpresslineLike()'
-- vim.o.statusline = '%!v:lua.Compact()'
