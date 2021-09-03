-- My neovim standard lib :)

require "nvim"

vim.g.mapleader = " "

-- Theme
vim.colorscheme "gruvbuddy"
-- vim.colorscheme "sitruuna"
-- vim.colorscheme "dracula"

-- Fuzzy finder
-- vim.g.fuzzy_finder = "fzf"
vim.g.fuzzy_finder = "telescope"

-- Load plugins and configuration
require "plugins"
