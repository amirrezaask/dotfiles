local dap = require'dap'

dap.adapters.go = function(callback, config)
  local port = 38697
  local handle
  local pid_or_err
  handle, pid_or_err =
      vim.loop.spawn(
      "dlv",
      {
        args = {"dap", "-l", "127.0.0.1:" .. port},
        detached = true
      },
      function(code)
        handle:close()
        print("Delve exited with exit code: " .. code)
      end
    )
    vim.defer_fn(
      function()
        dap.repl.open()
        callback({type = "server", host = "127.0.0.1", port = port})
      end,
    100)
  callback({type = "server", host = "127.0.0.1", port = port})
end
dap.configurations.go = {
  {
    type = "go",
    name = "Debug",
    request = "launch",
    program = "${file}"
  }
}

vim.cmd [[nnoremap <silent> <F3> :lua require'dap'.toggle_breakpoint()<CR>]]
vim.cmd [[nnoremap <silent> <F4> :lua require'dap'.repl.open()<CR>]]
vim.cmd [[nnoremap <silent> <F5> :lua require'dap'.continue()<CR>]]
vim.cmd [[nnoremap <silent> <F7> :lua require'dap'.step_into()<CR>]]
vim.cmd [[nnoremap <silent> <F8> :lua require'dap'.step_over()<CR>]]
vim.cmd [[nnoremap <silent> <F9> :lua require'dap'.step_out()<CR>]]
vim.cmd [[nnoremap <silent> <F10> :lua require('dap.ui.variables').hover()<CR>]]
