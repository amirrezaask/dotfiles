-- My neovim standard lib :)
require('nvim')

vim.g.mapleader = ' '

-- If you want transparency support
vim.g.transparent = false

-- colorscheme
require('colors.gruvbuddy')

-- Load plugins and configuration
require('plugins')
