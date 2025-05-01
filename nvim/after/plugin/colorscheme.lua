vim.g.gruvbuddy_style = "dark"

if Has("rose-pine") then
  ---@diagnostic disable-next-line: missing-fields
  require("rose-pine").setup({
    styles = {
      italic = false,
    },
  })
end

if Has("gruvbox") then
  require("gruvbox").setup({
    contrast = "hard",
  })
end

vim.cmd.colorscheme("gruvbuddy")
