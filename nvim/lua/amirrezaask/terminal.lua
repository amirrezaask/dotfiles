local M = {}
local nvim = require('amirrezaask.nvim')

function M.open(opts)
  opts = opts or {}
  opts.orientation = opts.orientation or 'v'
  if opts.orientation == 'v' then
    vim.cmd([[ vnew | term ]])
  else
    vim.cmd([[ new | term ]])
  end
end
nvim.command('VTerm', [[ lua require('amirrezaask.terminal').open({ orientation = 'v' })]])
nvim.command('Term', [[ lua require('amirrezaask.terminal').open({ orientation = 'h' })]])
return M
