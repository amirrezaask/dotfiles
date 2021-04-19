-- init.lua
local nvim = require('amirrezaask.nvim')

-- Load Lua helpers
require('ftplugin.lua')

-- Install Plugins
require('plugin.plugins')

require('plugin.fuzzy')
-- require'plugin.telescope'
-- require'plugin.fzf'

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
local base16 = require('base16')
local theme_names = base16.theme_names()
base16_position = 1
function cycle_theme()
  base16_position = (base16_position % #theme_names) + 1
  print(theme_names[base16_position])
  base16(base16.themes[theme_names[base16_position]], true)
end
nvim.command('Base16Cycle', cycle_theme)
require('amirrezaask.listchars'):update()
require('plugin.mappings')
require('plugin.lsp')
require('plugin.expressline')
require('plugin.dap')
require('plugin.snippets')
require('plugin.treesitter')
require('base16')(require('base16').themes.norcalli, true)
-- [[
-- phd
-- monokai
-- chalk
-- black-metal-bathory
-- circus
-- harmonic-dark
-- classic-dark
-- material-darker
-- helios
-- material
-- outrun-dark
-- snazzy
-- ia-dark
-- solarized-dark
-- spacemacs
-- default-dark
-- synth-midnight-dark
-- material-palenight
-- ]]
-- require('colorbuddy').colorscheme('gruvbuddy')
nvim.command('Base16Editor', [[lua require'base16.editor'.open(require'base16'.themes["<args>"])]], 1)

-- Register commands
nvim.command('ColorizerColorPicker', function()
  require('colorizer').color_picker_on_cursor()
end)
nvim.command('VTerm', [[ vnew | term ]])
nvim.command('Term', [[ new | term ]])
