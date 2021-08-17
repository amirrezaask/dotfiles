-- My neovim standard lib :)

require "nvim"

vim.g.mapleader = " "

-- Theme
COLOR = "gruvbuddy"
vim.colorscheme(COLOR)

-- Load plugins and configuration
require "plugins"
