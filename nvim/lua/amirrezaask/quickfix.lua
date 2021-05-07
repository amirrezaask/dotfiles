local M = {}
local nvim = require('amirrezaask.nvim')

nvim.mode_map({
  n = {
    ['{'] = ':cprev<CR>',
    ['}'] = ':cnext<CR>',
  },
})

function M.quickfix_entry(filename, lnum, col)
  return {filename=filename, lnum=lnum, col=col}
end

function M.set_qflist(results)
  vim.fn.setqflist(results)
end


return M
