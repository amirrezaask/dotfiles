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

-- Load packer.nvim plugins
require('plugins')

-- Loads all lua files in lua/plugin directories
LoadLuaPlugins()

