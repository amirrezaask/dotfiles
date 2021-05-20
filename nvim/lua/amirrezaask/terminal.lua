local M = {}
local floating = require('amirrezaask.floating')

function M.open(opts)
  opts = opts or {}
  opts.orientation = opts.orientation or 'v'
  if opts.orientation == 'v' then
    vim.cmd([[ vnew | term ]])
  else
    vim.cmd([[ 15new | term ]])
  end
end

function M.floating(opts)
  opts = opts or {}
  opts.width_pct = opts.width_pct or 80
  opts.height_pct = opts.height_pct or 50

  local buf, win = floating:new(opts)
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_set_current_win(win)
  vim.cmd [[ startinsert! ]]
  vim.cmd [[ term ]]
end

vim.command('VTerm', function()
  require('amirrezaask.terminal').open({ orientation = 'v' })
end)

vim.command('Term', function()
  require('amirrezaask.terminal').open({ orientation = 'h' })
end)

return M
