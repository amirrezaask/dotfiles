local nvim = require('amirrezaask.nvim')
vim.g.mapleader = ' '


nvim.map {
  ['<C-j>'] = '<C-w>j',
  ['<C-k>'] = '<C-w>k',
  ['<C-l>'] = '<C-w>l',
  ['<C-h>'] = '<C-w>h',
  Q = '<nop>',
  [';'] = ':',
  ['<f5>'] = '<cmd> Run<CR>',
  ['<Left>'] = '<cmd>vertical resize -5<CR>',
  ['<Right>'] = '<cmd>vertical resize +5<CR>',
  ['<Up>'] = '<cmd>resize +5<CR>',
  ['<Down>'] = '<cmd>resize -5<CR>',
}


nvim.map {
  ['n <CR>'] = function()
    if vim.v.hlsearch == 1 then
      vim.cmd [[ :nohl ]]
    end
    vim.fn.feedkeys("<CR>")
  end,
  ['n tn'] = ':tabnext<CR>',
  ['n tp'] = ':tabprevious<CR>',
  ['n tc'] = ':tabclose<CR>',
  ['n tt'] = ':tabnew<CR>',
  ['n <leader>v'] = '<cmd>vnew<CR>',
  ['n j'] = 'gj',
  ['n k'] = 'gk',
  ['t <Esc>'] = '<C-\\><C-n>',
  ['t jk'] = '<C-\\><C-n>',
  ['t jj'] = '<C-\\><C-n>',
  ['t kk'] = '<C-\\><C-n>',
  ['t kj'] = '<C-\\><C-n>',
  ['i jk'] = '<esc>',
  ['i jj'] = '<esc>',
  ['i kk'] = '<esc>',
  ['i kj'] = '<esc>',
}
