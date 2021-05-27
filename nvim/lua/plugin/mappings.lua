vim.g.mapleader = ' '
vim.map {
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


vim.map {
  ['n <CR>'] = function()
    if vim.v.hlsearch == 1 then
      vim.c.nohl()
    end
    vim.fn.feedkeys("<CR>")
  end,
  ['<M-p>'] = ':tabprevious<CR>',
  ['<M-n>'] = ':tabnext<CR>',
  ['<M-q>'] = ':tabclose<CR>',
  ['<M-t>'] = ':tabnew<CR>',
  ['n tn'] = ':tabnext<CR>',
  ['n tp'] = ':tabprevious<CR>',
  ['n tc'] = ':tabclose<CR>',
  ['n tt'] = ':tabnew<CR>',
  ['n <leader>v'] = '<cmd>vnew<CR>',
  ['n j'] = 'gj',
  ['n k'] = 'gk',
  ['n ,t'] = require('plugin.terminal').floating,
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

vim.map {
  ['n ,b'] = require('blame').blame,
  ['n ,c'] = require('blame').clear
}
