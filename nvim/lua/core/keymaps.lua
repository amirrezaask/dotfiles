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
  vim.keymap.set("v", lhs, rhs, opts)
end
