local lua = {}
local nvim = require('amirrezaask.nvim')

-- Force reload the module
function R(mod)
  package.loaded[mod] = nil
  return require(mod)
end

-- is the package loaded ?
function L(mod)
  return package.loaded[mod] ~= nil
end

-- Printer
function P(obj)
  print(vim.inspect(obj))
end

-- Eval line
function E()
  local filetype = vim.api.nvim_buf_get_option(0, 'filetype')
  local line = vim.fn.getline('.')
  if filetype == 'lua' then
    vim.cmd(string.format([[ lua %s ]], line))
  end
end

function lua.format(opts)
  if vim.fn.executable('stylua') ~= 1 then
    print('Error: stylua not found')
    return
  end
  opts = opts or {}
  opts.config = opts.config or os.getenv('HOME') .. '/.stylua.toml'
  local filename = vim.api.nvim_buf_get_name(0)
  vim.cmd([[ w ]])
  vim.cmd(string.format([[ silent ! stylua --config-path %s %s ]], opts.config, filename))
  vim.cmd([[ e ]])
end

nvim.command('LuaFormat', lua.format)

nvim.augroup({
  lua = {
    'BufEnter',
    '*.lua',
    'set ts=2 sw=2 sts=2 expandtab',
  },
})
function lua.enable_autoformat()
  vim.cmd([[
    autocmd BufWritePre <buffer> lua require'ftplugin.lua'.format()
  ]])
end

vim.cmd([[ nnoremap ,af <cmd>lua require'ftplugin.lua'.enable_autoformat()<CR> ]])

vim.api.nvim_buf_set_keymap(0, 'n', ',f', '<cmd>LuaFormat<CR>', { noremap = true })

require('amirrezaask.nvim').mode_map({
  n = {
    ['<leader>x'] = '<cmd>lua EVAL()<CR>',
    ['<leader>X'] = '<cmd>luafile %<CR>',
  },
})

return lua
