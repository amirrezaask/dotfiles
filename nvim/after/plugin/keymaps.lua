local nnoremap = require("amirreza.keymap").nnoremap
local inoremap = require("amirreza.keymap").inoremap
local vnoremap = require("amirreza.keymap").vnoremap
local tnoremap = require("amirreza.keymap").tnoremap

-- Set <Space> as <leader>
vim.g.mapleader = ' '
-- basic keybindings
nnoremap("Q", '<NOP>')
nnoremap(";,", ':')
nnoremap('q;', 'q:')

nnoremap('<Left>', ':vertical resize -5<CR>')
nnoremap('<Right>', ':vertical resize +5<CR>')
nnoremap('<Up>', ':resize +5<CR>')
nnoremap('<Down>', ':resize -5<CR>')

nnoremap('j', 'gj')
nnoremap('k', 'gk')

tnoremap('<Esc>', '<C-\\><C-n>')
tnoremap('jk','<C-\\><C-n>')
tnoremap('kj','<C-\\><C-n>')

inoremap('jk', '<esc>')
inoremap('kj', '<esc>')

nnoremap('Y', 'y$')
nnoremap('n', 'nzz')
nnoremap('N', 'Nzz')

nnoremap('<M-p>', ':bprev<CR>')
nnoremap('<M-n>', ':bnext<CR>')

nnoremap('<M-j>', ':m .+1<CR>==')
nnoremap('<M-k>', ':m .-2<CR>==')

inoremap('<M-j>', '<Esc>:m .+1<CR>==gi')
inoremap('<M-k>', '<Esc>:m .-2<CR>==gi')

vnoremap('<M-k>', ':m >+1<CR>gv=gv')
vnoremap('<M-k>', '<Esc>:m .-2<CR>==gi')

nnoremap('{', ':cprev<CR>')
nnoremap('}', ':cnext<CR>')

nnoremap("<C-h>", ":tabprev<CR>")
nnoremap("<C-l>", ":tabnext<CR>")
nnoremap("<C-n>", ":tabnew<CR>")


vim.cmd [[ nnoremap <expr><CR> {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]]


