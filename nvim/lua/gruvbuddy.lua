local hl = require('palette').highlight

-- Extracted from https://github.com/tjdevries/gruvbuddy.nvim
hl:apply { "Normal", bg='#282c34', fg='#e0e0e0' }
vim.g.transparent = true
if vim.g.transparent then
  hl:apply { { "Normal", "LineNr", "CursorLineNR", "SignColumn" }, bg='none' }
end
hl:apply { "InvNormal", bg="#c5c8c6", fg="#282c34" }
hl:apply { "NormalFloat", bg="#111317", fg="#fafafa" }
hl:apply { "LineNr", bg="#282a2e", fg="#969896" }
hl:apply { "EndOfBuffer", fg="#969896"}
hl:apply { "SignColumn", bg="#282a2e", fg="#969896" }
hl:apply { "Visual", bg="#38556d" }
hl:apply { "VisualMode", bg="#38556d" }
hl:apply { "VisualLineMode", bg="#38556d" }
hl:apply { "Cursor", bg="#e0e0e0", fg="#282c34" }
hl:apply { "CursorLine", bg="#333842"}
hl:apply { "PMenu", bg="#373b41", fg="#b4b7b4"}
hl:apply { "PMenuSel", bg="#fbffad", fg="#282c34"}
hl:apply { "PMenuSbar", bg="#282c34"}
hl:apply { "PMenuThumb", bg="#b4b7b4"}
hl:apply { "Search", bg="#f8fe7a", fg="#282a2e"}
hl:apply { "TabLine", bg="#282a2e", fg="#5f89ad"}
hl:apply { "TabLineFill", bg="#969896", fg="#ebdbb2"}
hl:apply { "TabLineSel", bg="#282a2e", fg="#ffffff"}
hl:apply { "ColorColumn", bg="#81a2be", fg="#ffffff"}
hl:apply { "qfFileName", fg="#f8fe7a" }

hl:apply { "Special", fg="#aa92cd", styles={"italic"} }

hl:apply { "SpecialChar", fg="#a3685a" }
hl:apply { "NonText", fg="#4e545c" }
hl:apply { "WhiteSpace", fg="#8e6fbd" }
hl:apply { "Conceal", bg="#4e545c", fg="#282c34" }

hl:apply { "StatusLine", bg="#81a2be", fg="#373b41", styles = {} }

hl:apply { "StatusLineNC", bg="#3f4349", fg="#969896" }

hl:apply { "User1", bg="#f8fe7a", fg="#ffffff" }
hl:apply { "User2", bg="#cc6666", fg="#ffffff" }
hl:apply { "User3", bg="#99cc99", fg="#ffffff" }
hl:apply { "CommandMode", link = "Constant" }
hl:apply { "VisualMode", link = "StatusLine" }
hl:apply { "NormalMode", link = "Function" }
hl:apply { "InsertMode", link = "StatusLine" }
hl:apply { "TerminalMode", link = "Constant" }

hl:apply { {"Boolean", "TSBoolean" }, fg="#7fbfff" }
hl:apply { "Error", fg="#d98c8c" }
hl:apply { { "Label", "TSLabel" } , fg="#cc6666" }
hl:apply { { "Operator", "TSOperator" }, fg="#e6b3b3" }
hl:apply { "PreProc", fg="#f8fe7a" }
hl:apply { { "Repeat", "TSRepeat" }, fg="#cc6666" }
hl:apply { "Repeat", fg="#cc6666" }
hl:apply { "Statement", fg="#c04040" }
hl:apply { "StorageClass", fg="#f8fe7a" }
hl:apply { { "String", "TSString" }, fg="#99cc99", styles = {"italic"} }
hl:apply { "Structure", fg="#b294bb" }
hl:apply { { "Tag", "TSTag" }, fg="#f8fe7a" }
hl:apply { "Todo", fg="#f8fe7a" }
hl:apply { "Typedef", fg="#f8fe7a" }
hl:apply { { "Type", "TSType" }, fg="#b294bb" }
hl:apply { "Folded", bg="#4e545c", fg="#7c7f7c" }
hl:apply { { "vimFunction", "Function", "TSFunction", "pythonBuiltinFunc" }, fg="#f8fe7a" }

hl:apply { { "Number", "Float", "TSNumber", "TSFloat" }, fg="#cc6666" }
hl:apply { "MatchParen", fg="#8abeb7" }
hl:apply { { "TSCharacter", "Character" }, fg="#cc6666" }
hl:apply { { "TSComment", "Comment" }, fg="#969896", styles={"italic"}}
hl:apply { { "Conditional", "TSConditional" }, fg="#f8fe8a", styles={"bold"}}
hl:apply { { "Identifier", "TSConstant", "Constant" }, fg="#cc6666" }

hl:apply { { "Define", "TSInclude", "Include" }, fg="#8abeb7" }
hl:apply { { "vimCommand", "vimNotFunc", "TSConstant", "vimLet" }, fg="#81a2be" }
hl:apply { "TSVariable", fg="#e0e0e0" }
hl:apply { "TSFunction", fg="#f8fe7a", styles={"bold","italic"}}
hl:apply { { "TSVariableBuiltin", "TSConstantBuiltin" }, fg="#f8fe7a" }
hl:apply { { "vimIsCommand", "vimFuncVar", "TSKeyword", "Keyword" }, fg="#b294bb", styles={"italic","bold"}}
hl:apply { "TSLabel", fg="#cc6666"}


hl:apply { "HelpDoc", bg="#698b69", fg="#ffffff" }
hl:apply { "HelpIgnore", fg="#99cc99" }

hl:apply { "gitDiff", fg="#c7c7c7" }
hl:apply { "DiffChange", bg="#2800" }
hl:apply { "DiffText", bg="#8e00" }
hl:apply { "DiffAdd", bg="#2800" }
hl:apply { "DiffDelete", bg="#0" }
hl:apply { "DiffRemoved", fg="#cc6666" }
hl:apply { "DiffAdded", fg="#99cc99" }

hl:apply { "VimwikiBold", fg="#cc6666" }
hl:apply { "TelescopeMatching", fg="#f2904b", styles={"bold"} }

hl:apply { "StartifyBracket", fg="#cc6666" }
hl:apply { "StartifyFile", fg="#c04040" }
hl:apply { "StartifyNumber", fg="#81a2be" }
hl:apply { "StartifyPath", fg="#77bb77" }
hl:apply { "StartifySlash", fg="#8abeb7" }
hl:apply { "StartifySection", fg="#fbffad" }
hl:apply { "StartifySpecial", fg="#de935f" }
hl:apply { "StartifyHeader", fg="#de935f" }
hl:apply { "StartifyFooter", fg="#373b41" }

hl:apply { "foldbraces", fg="#f2e5bc" }

hl:apply { "markdownH1", fg="#81a2be" }
hl:apply { "markdownH2", fg="#a3bbd0" }
hl:apply { "markdownH3", fg="#c5d4e1" }

hl:apply { "vimCommentTitle", fg="#cc6666" }
hl:apply { "vimMapModKey", fg="#8abeb7" }
hl:apply { "vimNotation", fg="#8abeb7" }
hl:apply { "vimMapLHS", fg="#f8fe7a" }
hl:apply { "vimNotation", fg="#8abeb7" }
hl:apply { "vimBracket", fg="#96535c" }
hl:apply { "vimMap", fg="#698b69" }
hl:apply { "nvimMap", fg="#698b69" }
hl:apply { "vimAutoloadFunction", fg="#f3fe14" }

