local set = vim.keymap.set

nnoremap("<C-d>", "<C-d>zz")
nnoremap("<C-u>", "<C-u>zz")

nnoremap("Q", "<NOP>")

nnoremap("{", ":cnext<CR>")
nnoremap("}", ":cprev<CR>")

nnoremap("Y", "y$")

nnoremap("n", "nzz")
nnoremap("N", "Nzz")

nnoremap("j", "gj")
nnoremap("k", "gk")

nnoremap("<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })

tnoremap("<Esc>", "<C-\\><C-n>")
tnoremap("jk", "<C-\\><C-n>")
tnoremap("kj", "<C-\\><C-n>")

inoremap("jk", "<esc>")
inoremap("kj", "<esc>")

set({ "n", "t" }, "<C-,>", "<cmd>tabprev<CR>")
set({ "n", "t" }, "<C-.>", "<cmd>tabnext<CR>")
