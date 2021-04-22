local nvim = require('amirrezaask.nvim')
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

vim.g.netrw_banner = 0
require('ftplugin.lua')
require('plugin.plugins')

nvim.colorscheme('gruvbox')
require('amirrezaask.terminal')
require('plugin.telescope')
require('mappings')
require('plugin.compe')
require('gitsigns').setup()
require('amirrezaask.listchars'):update()
require('plugin.mappings')
require('plugin.lsp')
require('amirrezaask.statusline')
require('plugin.dap')
require('plugin.treesitter')
require('amirrezaask.quickfix')
