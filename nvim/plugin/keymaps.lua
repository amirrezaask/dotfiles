vim.g.mapleader = " "

function nnoremap(lhs, rhs, opts)
  opts = opts or {}
  opts = vim.tbl_extend("force", opts, { noremap = true })
  vim.keymap.set("n", lhs, rhs, opts)
end

function inoremap(lhs, rhs, opts)
  opts = opts or {}
  opts = vim.tbl_extend("force", opts, { noremap = true })
  vim.keymap.set("i", lhs, rhs, opts)
end

function vnoremap(lhs, rhs, opts)
  opts = opts or {}
  opts = vim.tbl_extend("force", opts, { noremap = true })
  vim.keymap.set("v", lhs, rhs, opts)
end

function tnoremap(lhs, rhs, opts)
  opts = opts or {}
  opts = vim.tbl_extend("force", opts, { noremap = true })
  vim.keymap.set("t", lhs, rhs, opts)
end

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
set({ "n", "t" }, "<C-t>", "<cmd>tabnew<CR>")
