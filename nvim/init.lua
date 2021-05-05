-- Basic NeoVIM stuff
require('options')
require('mappings')

-- Personal Modules
require('ftplugin.lua')
require('amirrezaask.terminal')
require('amirrezaask.quickfix')
require('amirrezaask.listchars')
require('amirrezaask.statusline')

-- Plugins and configurations
require('plugin.plugins')
require('amirrezaask.nvim').colorscheme('gruvbox')
-- require('colorbuddy').colorscheme('gruvbuddy')
require('plugin.compe')
require('plugin.telescope')
require('plugin.colorizer')
require('plugin.gitsigns')
require('plugin.mappings')
require('plugin.snippets')
require('plugin.lsp')
require('plugin.dap')
require('plugin.treesitter')
