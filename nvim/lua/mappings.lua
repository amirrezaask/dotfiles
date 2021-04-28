local nvim = require('amirrezaask.nvim')

nvim.map({
  ['<C-j>'] = '<C-w>j',
  ['<C-k>'] = '<C-w>k',
  ['<C-l>'] = '<C-w>l',
  ['<C-h>'] = '<C-w>h',
  Q = '<nop>',
  [';'] = ':',
  ['<f5>'] = '<cmd> Run<CR>',
})

vim.cmd([[ nnoremap <expr><CR> {-> v:hlsearch ? ":nohl<CR>" : "<CR>"}() ]])
nvim.mode_map({
  n = {
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
