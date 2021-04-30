local M = {}
local nvim = require('amirrezaask.nvim')

nvim.mode_map({
  n = {
    ['{'] = ':cprev<CR>',
    ['}'] = ':cnext<CR>',
  },
})
return M
