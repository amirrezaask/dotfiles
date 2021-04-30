local snippets = require('snippets')
local nvim = require('amirrezaask.nvim')

nvim.mode_map({
  i = {
    ['<c-s>'] = '<cmd> lua require"snippets".expand_or_advance(1)<CR>',
  },
})

local go = require('plugin.snippets.go')
local lua = require('plugin.snippets.lua')

local snips = {}
snips.go = go
snips.lua = lua
snips._global = {
  date = os.date,
}

snippets.snippets = snips
