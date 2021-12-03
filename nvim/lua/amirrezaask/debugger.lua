vim.g.dap_virtual_text = true

-- Commands
vim.cmd [[ command! DapToggleBreakpoint lua require("dap").toggle_breakpoint) ]]
vim.cmd [[ command! DapReplOpen lua  require("dap").repl.open) ]]
vim.cmd [[ command! DapContinue lua  require("dap").continue) ]]
vim.cmd [[ command! DapStepInto lua  require("dap").step_into) ]]
vim.cmd [[ command! DapStepOver lua  require("dap").step_over) ]]
vim.cmd [[ command! DapStepOut lua require("dap").step_out) ]]
vim.cmd [[ command! DapHover lua require("dap.ui.variables").hover) ]]

vim.cmd [[ command! Dap lua require"amirrezaask.telescope".make_command_picker("Dap", {})() ]]

-- Mappings
vim.cmd [[ map <F3> lua require("dap").toggle_breakpoint ]]
vim.cmd [[ map <F4> lua require("dap").repl.open ]]
vim.cmd [[ map <F9> lua require("dap").continue ]]
vim.cmd [[ map <F7> lua require("dap").step_into ]]
vim.cmd [[ map <F8> lua require("dap").step_over ]]
-- vim.cmd [[ map <F9> lua require("dap").step_out ]]
vim.cmd [[ map <F10> lua require("dap.ui.variables").hover ]]
