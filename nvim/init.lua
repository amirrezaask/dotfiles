-- My neovim standard lib :)
require('nvim')

vim.g.mapleader = ' '

-- If you want transparency support
vim.g.transparent = true

-- colorscheme
require('colors.gruvbuddy')
-- require('base16').set('norcalli')

-- Load plugins and configuration
require('plugins')
