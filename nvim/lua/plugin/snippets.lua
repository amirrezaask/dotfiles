local snippets = require('snippets')
local nvim = require('amirrezaask.nvim')

nvim.mode_map({
  i = {
    ['<c-s>'] = '<cmd> lua require"snippets".expand_or_advance(1)<CR>',
  },
})

snippets.snippets = {
  _global = {
    date = os.date,
  },
  lua = {
    todo = '-- TODO(amirreza): $1',
  },
  go = {
    iferr = [[ if err != nil { 
      $1
    }
    ]]
  }
}
