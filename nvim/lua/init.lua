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
  relativenumber = true,
})
require('mappings')
vim.g.netrw_banner = 0
require('ftplugin.lua')
require('amirrezaask.terminal')
require('amirrezaask.quickfix')
require('amirrezaask.listchars'):update()
require('amirrezaask.statusline')

-- Plugins and configurations
-- nvim.colorscheme('gruvbox')
require('colorbuddy').colorscheme('gruvbuddy')
-- require('plugin.base16')
-- SetBas16Colorscheme('norcalli')
require('plugin.plugins')
require('plugin.telescope')
require('plugin.compe')
require('gitsigns').setup()
require('plugin.mappings')
require('plugin.snippets')
require('plugin.lsp')
require('plugin.dap')
require('plugin.treesitter')
