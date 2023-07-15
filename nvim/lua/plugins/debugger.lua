local function config()
  require("dapui").setup {
    layouts = {
      {
        elements = {
          {
            id = "watches",
            size = 1,
          },
        },
        position = "bottom",
        size = 10,
      },
    },
  }
  vim.keymap.set("n", "<F2>", ":lua require'dapui'.toggle()<CR>")
  vim.keymap.set("n", "<F5>", function()
    require("dap").continue()
  end)
  vim.keymap.set("n", "<F10>", ":lua require'dap'.step_over()<CR>")
  vim.keymap.set("n", "<F11>", ":lua require'dap'.step_into()<CR>")
  vim.keymap.set("n", "<F12>", ":lua require'dap'.step_out()<CR>")
  vim.keymap.set("n", "<F1>", ":lua require'dap'.toggle_breakpoint()<CR>")
  vim.keymap.set("n", "<F9>", ":lua require'dap'.repl.open()<CR>")

  local dap, dapui = require "dap", require "dapui"
  dap.listeners.after.event_initialized["dapui_config"] = function()
    dapui.open()
  end
  dap.listeners.before.event_terminated["dapui_config"] = function()
    dapui.close()
  end
  dap.listeners.before.event_exited["dapui_config"] = function()
    dapui.close()
  end

  require("nvim-dap-virtual-text").setup {}
end
return {
  "mfussenegger/nvim-dap",
  dependencies = {
    "rcarriga/nvim-dap-ui",
    "leoluz/nvim-dap-go",
    "theHamsta/nvim-dap-virtual-text",
  },
  config = config,
}
