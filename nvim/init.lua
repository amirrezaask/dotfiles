local nvim = require('amirrezaask.nvim')

-- Basic NeoVIM stuff
require('options')
require('mappings')

-- Personal Modules
require('ftplugin.lua')
require('amirrezaask.terminal')
require('amirrezaask.quickfix')
require('amirrezaask.listchars')
require('amirrezaask.statusline')
nvim.colorscheme('onedark')

-- Load plugins
require('plugin')
