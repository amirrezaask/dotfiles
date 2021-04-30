local M = {}
local nvim = require('amirrezaask.nvim')

nvim.mode_map({
  n = {
    ['<C-k>'] = ':cprev<CR>',
    ['<C-j>'] = ':cnext<CR>',
  },
})
return M
