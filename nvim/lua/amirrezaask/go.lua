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
vim.g.go_fmt_autosave = 1
vim.g.go_fmt_command = "goimports"
vim.g.go_gopls_enabled = false
Go_telescope_picker = require("amirrezaask.telescope").make_command_picker("Go", {})

vim.cmd [[ command! GoPicker lua Go_telescope_picker() ]]
