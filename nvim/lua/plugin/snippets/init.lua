local snippets = require('snippets')
local nvim = require('amirrezaask.nvim')

nvim.mode_map({
  i = {
    ['<c-s>'] = '<cmd> lua require"snippets".expand_or_advance(1)<CR>',
  },
})

snippets.snippets['_global'] = {
  date = os.date,
}
require('plugin.snippets.go')
require('plugin.snippets.lua')
