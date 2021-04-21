local M = {}
local nvim = require('amirrezaask.nvim')

__QUICKFIX_IS_OPEN = false

function M.toggle()
  if __QUICKFIX_IS_OPEN then
    __QUICKFIX_IS_OPEN = false
    vim.cmd([[ cclose ]])
  else
    __QUICKFIX_IS_OPEN = true
    vim.cmd([[ copen ]])
  end
end

nvim.command('QuickFixToggle', function()
  require('amirrezaask.quickfix').toggle()
end)

nvim.mode_map({
  n = {
    ['<C-k>'] = ':cprev<CR>',
    ['<C-j>'] = ':cnext<CR>',
    -- ['<C-q>'] = ':QuickFixToggle<CR>',
  },
})
return M
