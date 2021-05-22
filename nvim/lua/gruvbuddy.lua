local hl = require('palette').highlight

-- Extracted from https://github.com/tjdevries/gruvbuddy.nvim
hl:apply { "Normal", bg='#282c34', fg='#e0e0e0' }

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
hl:apply { "ColorColumn", bg="#3131bf", fg="#ffffff"}
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
hl:apply { "CommandMode", bg="#99cc99", fg="#ffffff" }
hl:apply { "NormalMode", bg="#cc6666", fg="#ffffff" }
hl:apply { "InsertMode", bg="#f8fe7a", fg="#ffffff" }
hl:apply { "ReplaceMode", bg="#f8fe7a", fg="#ffffff" }
hl:apply { "TerminalMode", bg="#698b69", fg="#ffffff" }
hl:apply { "Boolean", fg="#de935f" }
hl:apply { "Comment", fg="#969896", styles={"italic"}}
hl:apply { "Character", fg="#cc6666" }
hl:apply { "Conditional", fg="#cc6666" }
hl:apply { "Define", fg="#8abeb7" }
hl:apply { "Error", fg="#d98c8c" }
hl:apply { "Number", fg="#cc6666" }
hl:apply { "Float", fg="#cc6666" }
hl:apply { "Constant", fg="#de935f" }
hl:apply { "Identifier", fg="#cc6666" }
hl:apply { "Include", fg="#8abeb7" }
hl:apply { "Keyword", fg="#b294bb" }
hl:apply { "Label", fg="#f8fe7a" }
hl:apply { "Operator", fg="#e6b3b3" }
hl:apply { "PreProc", fg="#f8fe7a" }
hl:apply { "Repeat", fg="#cc6666" }
hl:apply { "Repeat", fg="#cc6666" }
hl:apply { "Statement", fg="#c04040" }
hl:apply { "StorageClass", fg="#f8fe7a" }
hl:apply { "String", fg="#99cc99" }
hl:apply { "Structure", fg="#b294bb" }
hl:apply { "Tag", fg="#f8fe7a" }
hl:apply { "Todo", fg="#f8fe7a" }
hl:apply { "Typedef", fg="#f8fe7a" }
hl:apply { "Type", fg="#b294bb" }
hl:apply { "Folded", bg="#4e545c", fg="#7c7f7c" }
hl:apply { "Function", fg="#f8fe7a" }
hl:apply { "pythonBuiltinFunc", fg="#f8fe7a" }
hl:apply { "vimFunction", fg="#f8fe7a"}
hl:apply { "MatchParen", fg="#8abeb7"}

hl:apply { "HelpDoc", bg="#698b69", fg="#ffffff" }
hl:apply { "HelpIgnore", fg="#99cc99" }

hl:apply { "gitDiff", fg="#c7c7c7" }
hl:apply { "DiffChange", bg="#2800" }
hl:apply { "DiffText", bg="#8e00" }
hl:apply { "DiffAdd", bg="#2800" }
hl:apply { "DiffDelete", bg="#0" }
hl:apply { "DiffRemoved", fg="#cc6666" }
hl:apply { "DiffAdded", fg="#99cc99" }

hl:apply { "TSBoolean", fg="#3fffff" }
hl:apply { "TSCharacter", fg="#99cc99" }
hl:apply { "TSComment", fg="#969896", styles={"italic"}}
hl:apply { "TSConditional", fg="#f8fe8a", styles={"bold"}}
hl:apply { "TSConstant", fg="#cc6666" }
hl:apply { "TSInclude", fg="#8abeb7" }
hl:apply { "TSConstant", fg="#81a2be" }
hl:apply { "TSVariable", fg="#e0e0e0" }
hl:apply { "TSFunction", fg="#f8fe7a", styles={"bold","italic"}}
hl:apply { "TSVariableBuiltin", fg="#f8fe7a" }
hl:apply { "TSKeyword", fg="#b294bb", styles={"italic","bold"}}
hl:apply { "TSLabel", fg="#cc6666"}
hl:apply { "TSString", fg="#99cc99"}


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

hl:apply { "vimNotFunc", fg="#81a2be" }
hl:apply { "vimCommand", fg="#81a2be" }
hl:apply { "vimLet", fg="#aa92cd", styles={"italic"}}
hl:apply { "vimFuncVar", fg="#8e6fbd", styles={"italic"}}
hl:apply { "vimCommentTitle", fg="#cc6666" }
hl:apply { "vimIsCommand", fg="#aa92cd" }
hl:apply { "vimMapModKey", fg="#8abeb7" }
hl:apply { "vimNotation", fg="#8abeb7" }
hl:apply { "vimMapLHS", fg="#f8fe7a" }
hl:apply { "vimNotation", fg="#8abeb7" }
hl:apply { "vimBracket", fg="#96535c" }
hl:apply { "vimMap", fg="#698b69" }
hl:apply { "nvimMap", fg="#698b69" }
hl:apply { "vimAutoloadFunction", fg="#f3fe14" }

