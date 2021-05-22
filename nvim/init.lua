require('nvim')

-- Basic Neovim stuff
require('options')
require('mappings')

-- Personal Modules

require('amirrezaask.terminal')
require('amirrezaask.quickfix')
require('amirrezaask.listchars')
require('amirrezaask.statusline')

vim.g.transparent = true
vim.c.colorscheme('gruvbuddy')

-- Load plugins
require('plugin')
