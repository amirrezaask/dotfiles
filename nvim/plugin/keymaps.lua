vim.g.mapleader = " "
vim.g.maplocalleader = ","

local keymap = vim.keymap.set
keymap("n", "Y", "^v$y", { desc = "Copy whole line" })
keymap("t", "<esc>", [[<C-\><C-n>]])
keymap("i", "<C-c>", "<esc>")
keymap("n", "<C-d>", "<C-d>zz")
keymap("n", "<C-u>", "<C-u>zz")
keymap({ "n" }, "<C-j>", "<C-w>j") -- Window navigation
keymap({ "n" }, "<C-k>", "<C-w>k") -- Window navigation
keymap({ "n" }, "<C-h>", "<C-w>h") -- Window navigation
keymap({ "n" }, "<C-l>", "<C-w>l") -- Window navigation

keymap({ "t" }, "<C-j>", "<C-\\><C-n><C-w>j", { noremap = true }) -- Window navigation
keymap({ "t" }, "<C-k>", "<C-\\><C-n><C-w>k", { noremap = true }) -- Window navigation
keymap({ "t" }, "<C-h>", "<C-\\><C-n><C-w>h", { noremap = true }) -- Window navigation
keymap({ "t" }, "<C-l>", "<C-\\><C-n><C-w>l", { noremap = true }) -- Window navigation
keymap("n", "n", "nzz")
keymap("n", "N", "Nzz")
keymap("i", "jk", "<ESC>")
keymap("i", "kj", "<ESC>")
-- keymap("t", "jk", "<C-\\><C-n>", { noremap = true })
-- keymap("t", "kj", "<C-\\><C-n>", { noremap = true })
keymap("n", "<CR>", function() -- Taken from https://github.com/tjdevries/config.nvim/blob/master/plugin/keymaps.lua#L16
  if vim.v.hlsearch == 1 then
    vim.cmd.nohl()
    return ""
  else
    ---@diagnostic disable-next-line: undefined-field
    return vim.keycode "<CR>"
  end
end, { expr = true })
keymap("n", "j", "gj")
keymap("n", "k", "gk")
keymap("n", "{", "<cmd>cprev<CR>")
keymap("n", "}", "<cmd>cnext<CR>")
keymap({ "n", "t" }, "<M-k>", "<cmd>wincmd q<CR>")
keymap("n", "<C-q>", function()
  local wins = vim.api.nvim_list_wins()
  for _, win in ipairs(wins) do
    local buf = vim.api.nvim_win_get_buf(win)
    if vim.api.nvim_get_option_value("buftype", { buf = buf }) == "quickfix" then
      vim.cmd.cclose()
      return
    end
  end
  vim.cmd.copen()
end)
