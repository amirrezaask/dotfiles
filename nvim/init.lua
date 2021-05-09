-- Basic NeoVIM stuff
require('options')
require('mappings')

-- Personal Modules
require('ftplugin.lua')
require('amirrezaask.terminal')
require('amirrezaask.quickfix')
require('amirrezaask.listchars')
require('amirrezaask.statusline')
require('amirrezaask.nvim').colorscheme('gruvbuddy')

-- Load plugins
require('plugin')
