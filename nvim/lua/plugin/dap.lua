local has_dap, dap = pcall(require,'dap')
if not has_dap then return end

vim.g.dap_virtual_text = true
dap.adapters.go = function(callback, _)
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
vim.c("DapToggleBreakpoint", require'dap'.toggle_breakpoint)
vim.c("DapReplOpen", require'dap'.repl.open)
vim.c("DapContinue", require'dap'.continue)
vim.c("DapStepInto", require'dap'.step_into)
vim.c("DapStepOver", require'dap'.step_over)
vim.c("DapStepOut", require'dap'.step_out)
vim.c("DapHover", require('dap.ui.variables').hover)

-- Mappings
vim.map {
  ['<F3>'] = require'dap'.toggle_breakpoint,
  ['<F4>'] = require'dap'.repl.open,
  ['<F5>'] = require'dap'.continue,
  ['<F7>'] = require'dap'.step_into,
  ['<F8>'] = require'dap'.step_over,
  ['<F9>'] = require'dap'.step_out,
  ['<F10>'] = require'dap.ui.variables'.hover
}
