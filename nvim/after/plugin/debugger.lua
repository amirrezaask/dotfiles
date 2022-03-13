vim.g.dap_virtual_text = true

-- Commands
vim.cmd [[ command! DapToggleBreakpoint lua require("dap").toggle_breakpoint() ]]
vim.cmd [[ command! DapReplOpen lua  require("dap").repl.open() ]]
vim.cmd [[ command! DapContinue lua  require("dap").continue() ]]
vim.cmd [[ command! DapStepInto lua  require("dap").step_into() ]]
vim.cmd [[ command! DapStepOver lua  require("dap").step_over() ]]
vim.cmd [[ command! DapStepOut lua require("dap").step_out() ]]
vim.cmd [[ command! DapHover lua require("dap.ui.variables").hover() ]]

-- Mappings
vim.cmd [[ map <F3> DapToggleBreakpoint<CR> ]]
vim.cmd [[ map <F4> DapReplOpen<CR> ]]
vim.cmd [[ map <F7> DapStepInto<CR> ]]
vim.cmd [[ map <F8> DapStepOver<CR> ]]
vim.cmd [[ map <F9> DapContinue<CR> ]]
vim.cmd [[ map <F10> DapHover<CR> ]]

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
