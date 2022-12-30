require("tokyonight").setup {
  transparent = _G.transparent,
}
vim.keymap.set("n", "<leader>tt", function()
  vim.cmd.Telescope "colorscheme"
end)
pcall(vim.cmd.colorscheme, "tokyonight-night")
