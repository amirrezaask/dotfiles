-- init.lua
local nvim = require'nvim'

-- Install Plugins
require'plugins'

-- require'plugin.fuzzy'
require'plugin.telescope'

nvim.with_options {
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
  relativenumber = true,
}

local listchars = require'listchars'
listchars:update()

vim.cmd [[ set cursorline ]]
vim.cmd [[ set relativenumber ]]
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

-- Side tree
-- normal_maps['<Space>s'] = '<cmd>NvimTreeToggle<CR>'
normal_maps['<Space>s'] = '<cmd>lua require"sidetree".open_side_file_browser()<CR>'


-- Completion
insert_maps['<c-k>'] = '<Plug>(completion_prev_source)'
insert_maps['<c-j>'] = '<Plug>(completion_next_source)'
vim.g.completion_auto_change_source = true
vim.g.completion_chain_complete_list = {
  default = {
    {complete_items = {'lsp', 'snippet'}},
    {complete_items = {'tabnine'}},
    {complete_items = {'buffers'}},
    {mode= '<c-p>'},
    {mode= '<c-n>'}
  }
}
-- Git signs
require('gitsigns').setup()

-- Nvim dev helpers
require'dev'

-- Register keymaps
nvim.map(global_maps)

nvim.mode_map({
  n = normal_maps,
  i = insert_maps,
})

require'lsp'
vim.g.rustfmt_autosave = 1
vim.api.nvim_set_option("statusline", "[%l:%L] %m%f")
-- require('hardline').setup {}
-- statusline
require'el'.setup{}
-- Register commands
nvim.command('Base16Editor', [[lua require'base16.editor'.open(require'base16'.themes["<args>"])]], 1)
nvim.command('VTerm', [[ vnew | term ]])
nvim.command('Term', [[ new | term ]])

