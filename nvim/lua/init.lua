-- init.lua
local nvim = require'nvim'
-- Install Plugins
require'plugins'
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
  number = false,
}
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

-- Fuzzy.nvim configs
-- require'fuzzy_config'
require'fzf'
-- Telescope.nvim configs
-- require 'telescope_config'

-- Snippets
insert_maps['<c-k>'] = '<cmd> lua return require"snippets".expand_or_advance(1)<CR>'
insert_maps['<c-k>'] = '<cmd> lua return require"snippets".advance_snippet(-1)<CR>'

-- Git Blame
normal_maps['<Space>gm'] = '<cmd>GitMessenger<CR>'

local base16 = require'base16'
local fuzzy = require'fuzzy.lib' 

function Base16ThemeSelector()
  local theme_names = {}
  for k,_ in pairs(base16.themes) do
    table.insert(theme_names, k)
  end
  fuzzy.new {
    source = theme_names,
    handler = function(theme)
      for k, v in pairs(base16.themes) do
        if k == theme then
          base16(v)
        end
      end
    end
  }
end
vim.cmd [[ nnoremap <Space>ct <cmd>lua Base16ThemeSelector()<CR> ]]

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

require'base16'(require'base16'.themes["material-darker"])
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

require'dev'
-- Register keymaps
nvim.map(global_maps)

nvim.mode_map({
  n = normal_maps,
  i = insert_maps,
})

-- Statusline
-- vim.api.nvim_set_option("statusline", "%l:%L %m%f")

-- Register commands
nvim.command('Base16Editor', [[lua require'base16.editor'.open(require'base16'.themes["<args>"])]], 1)
nvim.command('VTerm', [[ vnew | term ]])
nvim.command('Term', [[ new | term ]])
