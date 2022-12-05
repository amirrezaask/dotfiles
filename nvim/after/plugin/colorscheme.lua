vim.g.catppuccin_flavour = "mocha"
local colorscheme = "gruvbox"
local transparent = true

local ok, gruvbox = pcall(require, "gruvbox")
if ok then
  gruvbox.setup {
    transparent_mode = transparent,
    contrast = "hard",
  }
end

ok, _ = pcall(require, "tokyonight")
if ok then
  require("tokyonight").setup {
    style = "night",
    transparent = transparent,
  }
end

pcall(function()
  vim.cmd("colorscheme " .. colorscheme)
end)

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
