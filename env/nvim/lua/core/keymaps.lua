local map = vim.keymap.set

map("i", "jk", "<esc>")
map("i", "kj", "<esc>")
map("i", "<C-c>", "<esc>")
map("n", "<C-d>", "<C-d>zz")
map("n", "<C-u>", "<C-u>zz")
map("n", "n", "nzz")
map("n", "N", "Nzz")
map("n", "<leader>i", ":edit $MYVIMRC<CR>")
