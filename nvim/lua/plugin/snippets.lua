local snippets = require('snippets')
local nvim = require'amirrezaask.nvim'

nvim.mode_map {
  i = {
    ['<c-k>'] = '<cmd> lua return require"snippets".expand_or_advance(1)<CR>',
    ['<c-j>'] = '<cmd> lua return require"snippets".advance_snippet(-1)<CR>'
  }
}

snippets.snippets = {
  _global = {
    date = os.date,
    todo = 'TODO',
  },
}
