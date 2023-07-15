vim.g.mapleader = " "

-- Copy/paste improvements
vim.keymap.set("n", "Y", "y$", { desc = "Copy line" }) -- Make yanking act like other operations
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]], { desc = "Copy into clipboard" }) -- Copy to clipboard
vim.keymap.set("n", "<leader>Y", [["+Y]], { desc = "Copy line into clipboard" })
vim.keymap.set("n", "<leader>p", [["+p]], { desc = "Paste from clipboard" })
-- If I visually select words and paste from clipboard, don't replace my
-- clipboard with the selected word, instead keep my old word in the clipboard
vim.keymap.set("x", "p", '"_dP')
-- Split windows
vim.keymap.set("n", "<Left>", "<cmd>vertical resize -10<CR>")
vim.keymap.set("n", "<Right>", "<cmd>vertical resize +10<CR>")
vim.keymap.set("n", "<C-w>=", "<cmd>wincmd =<CR>")
-- Simpler exiting insert mode
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
-- Quickfix list
vim.keymap.set({ "n" }, "{", "<cmd>cprev<CR>", { desc = "Previous quick fix list item" })
vim.keymap.set({ "n" }, "}", "<cmd>cnext<CR>", { desc = "Next quick fix list item" })
-- When moving around always have pointer centered in screen
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
-- Move lines
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")
vim.keymap.set("n", "Q", "<cmd>q<CR>")
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true }) -- handy when doing search in a buffer

-- Wrapped lines act as normal lines
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
-- Netrw
vim.keymap.set("n", "<leader>e", "<cmd>Ex<CR>")

-- Edit this file
vim.keymap.set("n", "<leader>i", "<cmd>edit ~/.config/nvim/init.lua<CR>", { desc = "Edit init.lua" })

local function open_term()
	vim.cmd([[ tabnew | term ]])
end

-- terminal emulator and tabs
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set({ "i", "n", "t" }, "<C-k>", "<cmd>tabnext<CR>")
vim.keymap.set({ "i", "n", "t" }, "<C-j>", "<cmd>tabprev<CR>")
vim.keymap.set({ "n", "t" }, "<A-w>", "<cmd>tabclose<CR>")
vim.keymap.set({ "i", "n", "t" }, "<C-,>", "<cmd>tabnew<CR>")
vim.keymap.set({"i", "n"}, "<C-`>", open_term)


