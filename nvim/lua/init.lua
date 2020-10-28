-- init.lua
local nvim = require'nvim'

-- Install Plugins
require'plugins'

nvim.with_options {
  ignorecase = true,
  modeline = true,
  autoread = true,
  cursorline = true,
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
  tabstop=4,
  shiftwidth=4,
  softtabstop=4,
  expandtab = true,
  backup = false,
  writebackup = false,
  swapfile = false,
  clipboard = 'unnamedplus',
}

-- TODO: fix this
vim.cmd [[ set cursorline ]]

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
  ['kj'] = '<esc>'
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


-- Fuzzy Searching
vim.g.fuzzy_options = {
  location = 'bottom|center',
  width = 45,
  height = 90
}

normal_maps['<Space><Space>'] = '<cmd>lua require("fuzzy").file_finder{depth=5}<CR>'
normal_maps['<Space>fg'] = '<cmd>lua require("fuzzy").git_files{}<CR>'
normal_maps['??'] = '<cmd>lua require("fuzzy").grep{}<CR>'
normal_maps['<Space>b'] = '<cmd>lua require("fuzzy").buffers{}<CR>'
normal_maps['?g'] = '<cmd>lua require("fuzzy").git_grep{}<CR>'
normal_maps['<Space>ec'] = '<cmd>lua require("fuzzy").file_finder{path="/home/amirreza/w/dotfiles"}<CR>'
normal_maps['<Space>en'] = '<cmd>lua require("fuzzy").file_finder{path="/home/.config/nvim"}<CR>'

-- Snippets
insert_maps['<c-k>'] = '<cmd> lua return require"snippets".expand_or_advance(1)<CR>'
insert_maps['<c-k>'] = '<cmd> lua return require"snippets".advance_snippet(-1)<CR>'

-- Git Blame
normal_maps['<Space>gm'] = '<cmd>GitMessenger<CR>'

-- LazyGit
vim.g.lazygit_floating_window_scaling_factor = 0.7
normal_maps['<Space>gg'] = '<cmd>LazyGit<CR>'

-- Netrw settings
vim.g.netrw_banner = 0

nvim.augroup{
  lua = {
    "BufEnter", '*.lua', 'set ts=2 sw=2 sts=2 expandtab'
  }
}

-- Register keymaps
nvim.map(global_maps)

nvim.mode_map({
  n = normal_maps,
  i = insert_maps
})
