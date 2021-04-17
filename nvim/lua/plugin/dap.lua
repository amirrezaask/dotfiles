local dap = require'dap'

vim.g.dap_virtual_text = true
dap.adapters.go = function(callback, config)
  local port = 38697
  local handle
  handle, _ =
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

-- Commands
vim.cmd [[command! DapToggleBreakpoint lua require'dap'.toggle_breakpoint()]]
vim.cmd [[command! DapReplOpen lua require'dap'.repl.open()]]
vim.cmd [[command! DapContinue lua require'dap'.continue()]]
vim.cmd [[command! DapStepInto lua require'dap'.step_into()]]
vim.cmd [[command! DapStepOver lua require'dap'.step_over()]]
vim.cmd [[command! DapStepOut lua require'dap'.step_out()]]
vim.cmd [[command! DapHover lua require('dap.ui.variables').hover()]]

-- Mappings
vim.cmd [[nnoremap <silent> <F3>  <cmd>DapToggleBreakpoint<CR>]]
vim.cmd [[nnoremap <silent> <F4>  <cmd>DapReplOpen<CR>]]
vim.cmd [[nnoremap <silent> <F5>  <cmd>DapContinue<CR>]]
vim.cmd [[nnoremap <silent> <F7>  <cmd>DapStepInto<CR>]]
vim.cmd [[nnoremap <silent> <F8>  <cmd>DapStepOver<CR>]]
vim.cmd [[nnoremap <silent> <F9>  <cmd>DapStepOut()<CR>]]
vim.cmd [[nnoremap <silent> <F10> <cmd>DapHover<CR>]]
