-- Basic Neovim stuff
require('options')
require('mappings')
-- Personal Modules
require('ftplugin.lua')
require('amirrezaask.terminal')
require('amirrezaask.quickfix')
require('amirrezaask.listchars')
require('amirrezaask.statusline')
vim.g.transparent = true
-- require'amirrezaask.nvim'.colorscheme('gruvbox')
require'amirrezaask.nvim'.colorscheme('gruvbuddy')
-- require('colorbuddy').colorscheme('gruvbuddy')

-- Load plugins
require('plugin')

