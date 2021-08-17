-- My neovim standard lib :)

require "nvim"

vim.g.mapleader = " "

-- Theme
COLOR = "tokyonight"
vim.colorscheme(COLOR)

-- Load plugins and configuration
require "plugins"
