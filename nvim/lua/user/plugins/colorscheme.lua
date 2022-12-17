vim.g.catppuccin_flavour = "macchiato"
vim.g.gruvbox_contrast_dark = "hard"
local tokyonight_style = "night"

local ok, gruvbox = pcall(require, "gruvbox")
if ok then
  gruvbox.setup {
    transparent_mode = transparent,
    contrast = "hard",
  }
end

ok, _ = pcall(require, "onedark")
if ok then
  require("onedark").setup {
    style = "darker",
    transparent = transparent,
  }
end

ok, _ = pcall(require, "tokyonight")
if ok then
  require("tokyonight").setup {
    style = tokyonight_style,
    transparent = transparent,
  }
end

pcall(function()
  vim.cmd("colorscheme " .. colorscheme)
end)
