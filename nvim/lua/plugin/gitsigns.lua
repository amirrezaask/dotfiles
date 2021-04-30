local nvim = require('amirrezaask.nvim')

require('gitsigns').setup({
  signs = {
    add = {  text = '+', numhl = 'GitSignsAddNr' },
    change = {  text = '~', numhl = 'GitSignsChangeNr' },
    delete = {  text = '_', numhl = 'GitSignsDeleteNr' },
    topdelete = {  text = '‾', numhl = 'GitSignsDeleteNr' },
    changedelete = {  text = '~-', numhl = 'GitSignsChangeNr' },
  },
})

nvim.mode_map({
  n = {
    ['<leader>gm'] = '<cmd>lua require"gitsigns".blame_line()<CR>',
  },
})
