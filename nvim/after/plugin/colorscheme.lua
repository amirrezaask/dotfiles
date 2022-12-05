vim.g.catppuccin_flavour = "mocha"
local colorscheme = "tokyonight"

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
    transparent = true,
  }
end

pcall(vim.cmd, "colorscheme " .. colorscheme)
