local nvim = require('amirrezaask.nvim')

nvim.mode_map({
  n = {
    ['<leader>gm'] = '<cmd>GitMessenger<CR>',
  },
})
