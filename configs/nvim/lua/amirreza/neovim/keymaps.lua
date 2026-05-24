vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.keymap.set("i", "jk", "<esc>")
vim.keymap.set("i", "kj", "<esc>")
vim.keymap.set("i", "<C-c>", "<esc>")

vim.keymap.set({ "n", "x", "o" }, "H", "^", { desc = "Start of Line" })
vim.keymap.set({ "n", "x", "o" }, "L", "g_", { desc = "End of Line" })

vim.keymap.set("v", "<", "<gv", { desc = "Indent Left" })
vim.keymap.set("v", ">", ">gv", { desc = "Indent Right" })
vim.keymap.set("v", "p", '"_dP', { desc = "Paste (no yank)" })

vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")

vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")

vim.keymap.set("n", "<leader>i", ":edit $MYVIMRC<CR>", { desc = "Edit Configuration" })
vim.keymap.set("n", "<leader>R", ":source $MYVIMRC<CR>", { desc = "Reload Configuration" })
vim.keymap.set("n", "<leader>t", ":edit ~/TODO.md<CR>", { desc = "Edit TODO.md" })
vim.keymap.set("n", "<C-q>", function()
  if vim.fn.getqflist({ winid = 0 }).winid ~= 0 then
    vim.cmd.cclose()
  else
    vim.cmd.copen()
  end
end, {
  desc = "Toggle quickfix list",
})
vim.keymap.set("n", "<leader>q", function() vim.diagnostic.setloclist { open = true } end)

vim.keymap.set("i", "<C-Space>", "<C-x><C-o>", { desc = "Trigger LSP completion" })
vim.keymap.set("n", "<CR>", function()
  if vim.v.hlsearch == 1 then
    vim.cmd.nohl()
    return ""
  end
  return vim.keycode("<CR>")
end, {
  expr = true,
})
