local M = {}
local floating = require('floating')

function M.open(opts)
  opts = opts or {}
  opts.orientation = opts.orientation or 'v'
  if opts.orientation == 'v' then
    vim.c['vnew | term']()
  else
    vim.c['15new | term']()
  end
end

function M.floating(opts)
  opts = opts or {}
  opts.width_pct = opts.width_pct or 80
  opts.height_pct = opts.height_pct or 50

  local buf, win = floating:new(opts)
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_set_current_win(win)
  vim.c.startinsert()
  vim.c.term()
end

vim.c('VTerm', function()
  require('plugin.terminal').open({ orientation = 'v' })
end)

vim.c('Term', function()
  require('plugin.terminal').floating()
end)

return M
