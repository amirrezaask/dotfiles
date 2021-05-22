require('nvim')
vim.g.mapleader = ' '

-- colorscheme
require('gruvbuddy')

-- Basic Neovim stuff
require('options')
require('mappings')

-- Personal Modules

require('amirrezaask.terminal')
require('amirrezaask.quickfix')
require('amirrezaask.listchars')
require('amirrezaask.statusline')

-- Load plugins
require('plugin')
