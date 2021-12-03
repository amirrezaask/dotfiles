require("go").setup()
local dap = require "dap"
dap.adapters.go = function(callback, _)
  local port = 38697
  local handle
  handle, _ = vim.loop.spawn("dlv", {
    args = { "dap", "-l", "127.0.0.1:" .. port },
    detached = true,
  }, function(code)
    handle:close()
    print("Delve exited with exit code: " .. code)
  end)
  vim.defer_fn(function()
    dap.repl.open()
    callback { type = "server", host = "127.0.0.1", port = port }
  end, 100)
  callback { type = "server", host = "127.0.0.1", port = port }
end

dap.configurations.go = {
  {
    type = "go",
    name = "Debug",
    request = "launch",
    program = "${file}",
  },
}

vim.cmd [[ command! GoCommands lua GO_telescope_picker() ]]

vim.cmd [[ augroup Go ]]
vim.cmd [[ autocmd BufWritePre *.go :silent! lua require('go.format').gofmt() ]]
vim.cmd [[ autocmd BufWritePre *.go :silent! lua require('go.format').goimport() ]]
vim.cmd [[ augroup END ]]
