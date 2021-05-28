vim.c.colorscheme('gruvbox')
local hl = require('palette').highlight
hl { "CommandMode", link = "Constant" }
hl { "VisualMode", link = "StatusLine" }
hl { "NormalMode", link = "Function" }
hl { "InsertMode", link = "StatusLine" }
hl { "TerminalMode", link = "Constant" }



