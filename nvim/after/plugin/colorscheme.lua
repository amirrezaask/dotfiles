vim.keymap.set("n", "<leader>tt", function()
  vim.cmd.Telescope "colorscheme"
end)
pcall(vim.cmd.colorscheme, "rose-pine")
