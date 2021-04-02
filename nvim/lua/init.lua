-- init.lua
local nvim = require'nvim'

-- Install Plugins
require'plugins'

nvim.with_options {
  ignorecase = true,
  modeline = true,
  autoread = true,
  cursorline = true,
  guicursor = '',
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
  splitright = true,
  splitbelow = true,
  cursorline = true,
}

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
  width = 50,
  height = 90,
  blacklist = {
    "vendor"
  }
}

normal_maps['<Space><Space>'] = '<cmd>lua require("fuzzy").file_finder{}<CR>'
normal_maps['<Space>fb'] = '<cmd>lua require("fuzzy").interactive_finder{}<CR>'
normal_maps['<Space>gf'] = '<cmd>lua require("fuzzy").git_files{}<CR>'
normal_maps['??'] = '<cmd>lua require("fuzzy").grep{}<CR>'
normal_maps['<Space>b'] = '<cmd>lua require("fuzzy").buffers{}<CR>'
normal_maps['<Space>gg'] = '<cmd>lua require("fuzzy").git_grep{}<CR>'
normal_maps['<Space>ec'] = '<cmd>lua require("fuzzy").file_finder{path="/home/amirreza/src/github.com/amirrezaask/dotfiles"}<CR>'
normal_maps['<Space>en'] = '<cmd>lua require("fuzzy").file_finder{path="/home/amirreza/.config/nvim"}<CR>'
normal_maps['<Space>c'] = '<cmd>lua require("fuzzy").commands{}<CR>'
normal_maps['<Space>h'] = '<cmd>lua require("fuzzy").history{}<CR>'
normal_maps['<Space>fr'] = '<cmd>MRU<CR>'
normal_maps['<Space>s'] = '<cmd>lua require("sidetree").open_side_file_browser()<CR>'
normal_maps['<Space>h'] = '<cmd>lua require("fuzzy").help{}<CR>'
normal_maps['<Space>gc'] = '<cmd>lua require("fuzzy").git_commits{}<CR>'
normal_maps['<Space>gb'] = '<cmd>lua require("fuzzy").git_bcommits{}<CR>'
normal_maps['<Space>gco'] = '<cmd>lua require("fuzzy").git_checkout{}<CR>'

-- Vim-Go
vim.g.go_fmt_command = "goimports"
vim.g.go_fmt_autosave = true


-- Snippets
insert_maps['<c-k>'] = '<cmd> lua return require"snippets".expand_or_advance(1)<CR>'
insert_maps['<c-k>'] = '<cmd> lua return require"snippets".advance_snippet(-1)<CR>'

-- Git Blame
normal_maps['<Space>gm'] = '<cmd>GitMessenger<CR>'

-- Netrw settings
vim.g.netrw_banner = 0

nvim.augroup{
  lua = {
    "BufEnter", '*.lua', 'set ts=2 sw=2 sts=2 expandtab'
  },
--  colorizer = {
 --   "BufEnter", '*', 'ColorizerAttachToBuffer'
  -- }
}

-- Completion
insert_maps['<c-k>'] = '<Plug>(completion_prev_source)'
insert_maps['<c-j>'] = '<Plug>(completion_next_source)'
vim.g.completion_auto_change_source = true
vim.g.completion_chain_complete_list = {
  default = {
    {complete_items= {'lsp', 'snippet'}},
    {complete_items = {'tabnine'}},
    {complete_items = {'buffers'}},
    {mode= '<c-p>'},
    {mode= '<c-n>'}
  }
}

-- Set statusline
vim.api.nvim_set_option("statusline", "%l:%L %m%f")

-- Register keymaps
nvim.map(global_maps)

nvim.mode_map({
  n = normal_maps,
  i = insert_maps,
  t = term_maps
})

-- Register commands
nvim.command('Base16Editor', [[lua require'base16.editor'.open(require'base16'.themes["<args>"])]], 1)
nvim.command('VTerm', [[ vnew | term ]])
nvim.command('Term', [[ new | term ]])
