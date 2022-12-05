vim.g.catppuccin_flavour = "mocha"
local colorscheme = "gruvbox"

local ok, gruvbox = pcall(require, "gruvbox")
if ok then
  gruvbox.setup {
    contrast = "hard",
  }
end

local ok, tokyonight = pcall(require, "tokyonight")
if ok then
  tokyonight.setup {
    style = "night",
  }
end

pcall(vim.cmd, "colorscheme " .. colorscheme)
