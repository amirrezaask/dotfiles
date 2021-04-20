-- init.lua
local nvim = require('amirrezaask.nvim')

-- Load Lua helpers
require('ftplugin.lua')

-- Install Plugins
require('plugin.plugins')

require('plugin.fuzzy')
-- require('plugin.telescope')

nvim.with_options({
  ignorecase = true,
  modeline = true,
  autoread = true,
  compatible = false,
  encoding = 'utf-8',
  hlsearch = true,
  history = 700,
  tabpagemax = 100,
  ruler = true,
  mouse = 'a',
  wrap = true,
  autoindent = true,
  termguicolors = true,
  tabstop = 4,
  shiftwidth = 4,
  softtabstop = 4,
  expandtab = true,
  backup = false,
  writebackup = false,
  swapfile = false,
  clipboard = 'unnamedplus',
  splitright = true,
  splitbelow = true,
  cursorline = true,
})

nvim.map({
  ['<C-j>'] = '<C-w>j',
  ['<C-k>'] = '<C-w>k',
  ['<C-l>'] = '<C-w>l',
  ['<C-h>'] = '<C-w>h',
  Q = '<nop>',
  [';'] = ':',
  ['<f5>'] = '<cmd> Run<CR>',
})

nvim.mode_map({
  n = {
    ['<Space>/'] = ':nohl<CR>',
    ['tn'] = ':tabnext<CR>',
    ['tp'] = ':tabprevious<CR>',
    ['tc'] = ':tabclose<CR>',
    ['tt'] = ':tabnew<CR>',
    ['<Space>v'] = '<cmd>vnew<CR>',
    j = 'gj',
    k = 'gk',
  },
  t = {
    ['<Esc>'] = '<C-\\><C-n>',
    ['jk'] = '<C-\\><C-n>',
    ['jj'] = '<C-\\><C-n>',
    ['kk'] = '<C-\\><C-n>',
    ['kj'] = '<C-\\><C-n>',
  },
  i = {
    ['jk'] = '<esc>',
    ['jj'] = '<esc>',
    ['kk'] = '<esc>',
    ['kj'] = '<esc>',
  },
})

-- Netrw settings
vim.g.netrw_banner = 0

-- compe.nvim
require('plugin.compe')

-- Git signs
require('gitsigns').setup()
require('amirrezaask.listchars'):update()
require('plugin.mappings')
require('plugin.lsp')
require('amirrezaask.statusline')
require('plugin.dap')
require('plugin.snippets')
require('plugin.treesitter')
require('amirrezaask.quickfix')
require('plugin.base16')

SetBas16Colorscheme('gruvbox-dark-hard')

-- Register commands
nvim.command('ColorizerColorPicker', function()
  require('colorizer').color_picker_on_cursor()
end)
nvim.command('VTerm', [[ vnew | term ]])
nvim.command('Term', [[ new | term ]])
