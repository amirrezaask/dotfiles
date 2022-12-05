vim.g.catppuccin_flavour = "mocha"
local colorscheme = "gruvbox"

local ok, gruvbox = pcall(require, "gruvbox")
if ok then
  gruvbox.setup {
    transparent_mode = true,
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

local hl = function(thing, opts)
  vim.api.nvim_set_hl(0, thing, opts)
end

hl("ColorColumn", {
  ctermbg = 0,
  bg = "#2B79A0",
})

hl("LineNr", {
  fg = "#5eacd3",
})

hl("netrwDir", {
  fg = "#5eacd3",
})
