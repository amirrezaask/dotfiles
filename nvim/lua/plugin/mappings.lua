local nvim = require('amirrezaask.nvim')

nvim.mode_map({
  n = {
    ['<Space>gm'] = '<cmd>GitMessenger<CR>',
  },
})
