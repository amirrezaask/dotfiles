let g:dap_virtual_text = v:true

" Commands
command! DapToggleBreakpoint lua require("dap").toggle_breakpoint) 
command! DapReplOpen lua  require("dap").repl.open) 
command! DapContinue lua  require("dap").continue) 
command! DapStepInto lua  require("dap").step_into) 
command! DapStepOver lua  require("dap").step_over) 
command! DapStepOut lua require("dap").step_out) 
command! DapHover lua require("dap.ui.variables").hover) 

" Mappings
map <F3> lua require("dap").toggle_breakpoint 
map <F4> lua require("dap").repl.open 
map <F9> lua require("dap").continue 
map <F7> lua require("dap").step_into 
map <F8> lua require("dap").step_over 
map <F10> lua require("dap.ui.variables").hover 
