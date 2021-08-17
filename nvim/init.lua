-- My neovim standard lib :)

require "nvim"

vim.g.mapleader = " "

-- Theme
vim.g.tokyonight_style = "night"
vim.colorscheme "tokyonight"

-- Load plugins and configuration
require "plugins"
