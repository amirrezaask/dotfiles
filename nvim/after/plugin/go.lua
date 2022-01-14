local dap = require "dap"
local Job = require "plenary.job"
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

local function format(bufnr)
  bufnr = bufnr or 0
  vim.cmd [[ write ]]
  local job = Job:new {
    "goimports",
    vim.api.nvim_buf_get_name(0),
  }

  local output = job:sync()

  if job.code ~= 0 then
    return
  end

  vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, output)
end

return { format = format }
