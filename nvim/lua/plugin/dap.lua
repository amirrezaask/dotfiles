local dap = require('dap')

vim.g.dap_virtual_text = true
dap.adapters.go = function(callback, config)
  local port = 38697
  local handle
  handle, _ = vim.loop.spawn('dlv', {
    args = { 'dap', '-l', '127.0.0.1:' .. port },
    detached = true,
  }, function(code)
    handle:close()
    print('Delve exited with exit code: ' .. code)
  end)
  vim.defer_fn(function()
    dap.repl.open()
    callback({ type = 'server', host = '127.0.0.1', port = port })
  end, 100)
  callback({ type = 'server', host = '127.0.0.1', port = port })
end
dap.configurations.go = {
  {
    type = 'go',
    name = 'Debug',
    request = 'launch',
    program = '${file}',
  },
}

-- Commands
vim.c([[DapToggleBreakpoint]], require'dap'.toggle_breakpoint)
vim.c([[DapReplOpen]], require'dap'.repl.open)
vim.c([[DapContinue]], require'dap'.continue)
vim.c([[DapStepInto]], require'dap'.step_into)
vim.c([[DapStepOver]], require'dap'.step_over)
vim.c([[DapStepOut]], require'dap'.step_out)
vim.c([[DapHover]], require('dap.ui.variables').hover)

-- Mappings
vim.nmap {
  ['<silent> <F3>'] = require'dap'.toggle_breakpoint,
  ['<silent> <F4>'] = require'dap'.repl.open,
  ['<silent> <F5>'] = require'dap'.continue,
  ['<silent> <F7>'] = require'dap'.step_into,
  ['<silent> <F8>'] = require'dap'.step_over,
  ['<silent> <F9>'] = require'dap'.step_out,
  ['<silent> <F10>'] = require'dap.ui.variables'.hover
}
