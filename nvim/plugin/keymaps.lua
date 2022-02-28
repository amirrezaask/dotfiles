vim.g.mapleader = " "

local map = function(mode, lhs, rhs) vim.api.nvim_set_keymap(mode, lhs, rhs, {silent=true, noremap=true}) end
map('n', "Q", "<NOP>")
map('n', ';', ':')
map('n', 'q;', 'q:')

map('n', '<Left>', ':vertical resize -5<CR>')
map('n', '<Right>', ':vertical resize +5<CR>')
map('n', '<Up>', ':resize +5<CR>')
map('n', '<Down>', ':resize -5<CR>')

map('n', 'j', 'gj')
map('n', 'k', 'gk')

map('t', '<Esc>', '<C-\\><C-n>')
map('t', 'jk', '<C-\\><C-n>')
map('t', 'kj', '<C-\\><C-n>')

map('i', 'jk', '<esc>')
map('i', 'kj', '<esc>')

map('n', 'Y', 'y$')
map('n', 'n', 'nzz')
map('n', 'N', '"Nzz')

map('n', ',tn', ":tabnext<CR>")
map('n', ',tp', ":tabprevious<CR>")
vim.cmd [[ 
  nnoremap <M-j> :m .+1<CR>==
  nnoremap <M-k> :m .-2<CR>==
  inoremap <M-j> <Esc>:m .+1<CR>==gi
  inoremap <M-k> <Esc>:m .-2<CR>==gi
  vnoremap <M-j> :m '>+1<CR>gv=gv
  vnoremap <M-k> :m '<-2<CR>gv=gv
]]

map('n', '{', ':cprev<CR>')
map('n', '}', ':cnext<CR>')


vim.cmd [[ nnoremap <expr><CR> {-> v:hlsearch ? ":nohl<CR>" : "<CR>"}() ]]


