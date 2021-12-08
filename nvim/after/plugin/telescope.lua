vim.api.nvim_set_keymap(
  "n",
  "<leader><leader>",
  '<cmd>lua require"amirrezaask.telescope".wrap("find_files", { hidden = false })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>fp",
  '<cmd>lua require"amirrezaask.telescope".wrap("find_files", {hidden = true, cwd = "~/.local/share/nvim/site/pack/packer"})<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>ps",
  '<cmd>lua require"amirrezaask.telescope".wrap("find_files", {hidden = true, cwd = "~/src/gitlab.snapp.ir/" })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<C-q>",
  '<cmd>lua require"amirrezaask.telescope".wrap("quickfix", {hidden = true })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "??",
  '<cmd>lua require"amirrezaask.telescope".wrap("live_grep", {hidden = true })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>en",
  '<cmd>lua require"amirrezaask.telescope".wrap("find_files", { hidden = true, cwd = "~/.config/nvim" })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "gd",
  '<cmd>lua require"amirrezaask.telescope".wrap("find_files", { hidden = true, cwd = "~/.config/nvim" })<CR>',
  { silent = true, noremap = true }
)
