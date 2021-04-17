-- init.lua
local nvim = require('amirrezaask.nvim')

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

-- vim.cmd([[ set cursorline ]])
local global_maps = {
  -- Easier window navigation
  ['<C-j>'] = '<C-w>j',
  ['<C-k>'] = '<C-w>k',
  ['<C-l>'] = '<C-w>l',
  ['<C-h>'] = '<C-w>h',
  Q = '<nop>',
  [';'] = ':',
  ['<f5>'] = '<cmd> Run<CR>',
}

local insert_maps = {
  -- Easily exit normal mode
  ['jk'] = '<esc>',
  ['jj'] = '<esc>',
  ['kk'] = '<esc>',
  ['kj'] = '<esc>',
}

local normal_maps = {
  ['<Space>/'] = ':nohl<CR>',
  ['tn'] = ':tabnext<CR>',
  ['tp'] = ':tabprevious<CR>',
  ['tc'] = ':tabclose<CR>',
  ['tt'] = ':tabnew<CR>',
  ['<Space>v'] = '<cmd>vnew<CR>',
  j = 'gj',
  k = 'gk',
}

-- Snippets
insert_maps['<c-k>'] = '<cmd> lua return require"snippets".expand_or_advance(1)<CR>'
insert_maps['<c-k>'] = '<cmd> lua return require"snippets".advance_snippet(-1)<CR>'

-- Git Blame
normal_maps['<Space>gm'] = '<cmd>GitMessenger<CR>'

-- Netrw settings
vim.g.netrw_banner = 0

nvim.augroup({
  lua = {
    'BufEnter',
    '*.lua',
    'set ts=2 sw=2 sts=2 expandtab',
  },
})

-- compe.nvim
require('plugin.compe')

-- Git signs
require('gitsigns').setup()

-- Register keymaps
nvim.map(global_maps)

-- Bind keys
nvim.mode_map({
  n = normal_maps,
  i = insert_maps,
})

require('amirrezaask.listchars'):update()
require('plugin.lsp')
require('plugin.expressline')
require('colorbuddy').colorscheme('gruvbuddy')
require('plugin.dap')
require('plugin.snippets')
require('plugin.treesitter')

-- Register commands
nvim.command('Base16Editor', [[lua require'base16.editor'.open(require'base16'.themes["<args>"])]], 1)
nvim.command('VTerm', [[ vnew | term ]])
nvim.command('Term', [[ new | term ]])
