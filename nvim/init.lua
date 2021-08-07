-- My neovim standard lib :)
require "nvim"

vim.g.mapleader = " "

-- If you want transparency support
vim.g.transparent = false

-- Theme
require "colors.gruvbuddy"
-- require('base16').set('norcalli')
-- require('colors.gruvbox')

-- Load plugins and configuration
require "plugins"
