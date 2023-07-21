local function config()
  require"toggleterm".setup {
    direction = "tab"
  }
  -- terminal emulator and tabs
  vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
  vim.keymap.set({ "i", "n", "t" }, "<C-k>", "<cmd>tabnext<CR>")
  vim.keymap.set({ "i", "n", "t" }, "<C-j>", "<cmd>tabprev<CR>")
  vim.keymap.set({ "i", "n", "t" }, "<C-,>", "<cmd>tabnew<CR>")
  vim.keymap.set({ "i", "n", "t" }, "<C-;>", vim.cmd.ToggleTerm)
end

return {
  {'akinsho/toggleterm.nvim', version = "*", config = config}
}
