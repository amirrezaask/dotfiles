vim.g.mapleader = " "
-- Copy/paste improvements
vim.keymap.set("n", "Y", "y$") -- Make yanking act like other operations
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]]) -- Copy to clipboard
vim.keymap.set("n", "<leader>Y", [["+Y]])
vim.keymap.set("n", "<leader>p", [["+p]])
-- Split windows
vim.keymap.set("n", "<leader>v", "<cmd>vsplit<CR>", { desc = "Split vertically" })
vim.keymap.set("n", "<leader>h", "<cmd>split<CR>", { desc = "Split horizontaly" })
vim.keymap.set({ "n", "i" }, "<C-l>", "<cmd>wincmd l<CR>", { desc = "Move to split right" })
vim.keymap.set({ "n", "i" }, "<C-h>", "<cmd>wincmd h<CR>", { desc = "Move to split left" })
vim.keymap.set("n", "<Left>", "<cmd>vertical resize -10<CR>")
vim.keymap.set("n", "<Right>", "<cmd>vertical resize +10<CR>")
vim.keymap.set("n", "<C-w>=", "<cmd>wincmd =<CR>")
-- Simpler exiting insert mode
vim.keymap.set({ "i" }, "<C-c>", "<esc>")
vim.keymap.set({ "t" }, "<C-c>", "<C-\\><C-n>")
vim.keymap.set("t", "<Esc>", "<C-\\><C-n>")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
-- Quickfix list
vim.keymap.set({ "n" }, "<C-k>", "<cmd>cprev<CR>", { desc = "Previous quick fix list item" })
vim.keymap.set({ "n" }, "<C-j>", "<cmd>cnext<CR>", { desc = "Next quick fix list item" })
-- When moving around always have pointer centered in screen
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
-- Move lines
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("n", "Q", "<NOP>")
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true }) -- handy when doing search in a buffer
