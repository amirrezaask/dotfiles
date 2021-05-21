-- Packer.nvim load plugins
require('plugin.plugins')

-- Load plugin configurations
require('blame').setup {
  always = false,
  prefix = ''
}
require('plugin.compe')
require('plugin.telescope')
require('plugin.colorizer')
require('plugin.gitsigns')
require('plugin.mappings')
require('plugin.snippets')
require('plugin.lsp')
require('plugin.dap')
require('plugin.treesitter')
require('plugin.fugitive')
require('numb').setup()

