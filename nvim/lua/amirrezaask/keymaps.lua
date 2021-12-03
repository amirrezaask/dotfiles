local nnoremap = function(lhs, rhs)
  vim.api.nvim_set_keymap("n", lhs, rhs, { silent = true, noremap = true })
end
local tnoremap = function(lhs, rhs)
  vim.api.nvim_set_keymap("t", lhs, rhs, { silent = true, noremap = true })
end
local inoremap = function(lhs, rhs)
  vim.api.nvim_set_keymap("i", lhs, rhs, { silent = true, noremap = true })
end
local vnoremap = function(lhs, rhs)
  vim.api.nvim_set_keymap("v", lhs, rhs, { silent = true, noremap = true })
end

-- Map leader to <Space>
vim.g.mapleader = " "

-- Simpler split movement
nnoremap("Q", "<NOP>")
nnoremap(";", ":")
nnoremap("q;", "q:")

-- Window resizes
nnoremap("<Left>", ":vertical resize -5<CR>")
nnoremap("<Right>", ":vertical resize +5<CR>")
nnoremap("<Up>", ":resize +5<CR>")
nnoremap("<Down>", ":resize -5<CR>")

nnoremap("j", "gj")
nnoremap("k", "gk")

tnoremap("<Esc>", "<C-\\><C-n>")
tnoremap("jk", "<C-\\><C-n>")
tnoremap("kj", "<C-\\><C-n>")

inoremap("jk", "<esc>")
inoremap("kj", "<esc>")

-- Move lines jetbrains style -> Thanks to TJ again
nnoremap("<M-j>", ":m .+1<CR>==")
nnoremap("<M-k>", ":m .-2<CR>==")

inoremap("<M-j>", "<Esc>:m .+1<CR>==gi")
inoremap("<M-k>", "<Esc>:m .-2<CR>==gi")

vnoremap("<M-j>", ":m '>+1<CR>gv=gv")
vnoremap("<M-k>", ":m '<-2<CR>gv=gv")

nnoremap("Y", "y$")
nnoremap("n", "nzz")
nnoremap("N", "Nzz")

nnoremap("{", ":cprev<CR>")
nnoremap("}", ":cnext<CR>")

-- Thanks to TJ again
vim.cmd [[ nnoremap <expr><CR> {-> v:hlsearch ? ":nohl<CR>" : "<CR>"}() ]]
