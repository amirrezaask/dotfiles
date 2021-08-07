local hl = require("palette").highlight
local styles = require("palette").styles

local colors = {
  light_gray = "#282c34",
  super_white = "#e0e0e0",
  white = "#c5c8c6",
  blue = "#38556d",
  black = "#111317",
  yellow = "#f8fe7a",
  red = "#cc6666",
  green = "#99cc99",
  dark_green = "#698b69",
}

-- Extracted from https://github.com/tjdevries/gruvbuddy.nvim
hl { "Normal", bg = colors.light_gray, fg = colors.white }
hl { "InvNormal", bg = colors.white, fg = colors.light_gray }
hl { "NormalFloat", bg = colors.black, fg = "#fafafa" }
hl { "LineNr", bg = "#282a2e", fg = "#969896" }
hl { "EndOfBuffer", fg = "#969896" }
hl { "SignColumn", bg = "#282a2e", fg = "#969896" }
hl { "Visual", bg = colors.blue }
hl { "VisualMode", bg = colors.blue }
hl { "VisualLineMode", bg = colors.blue }
hl { "Cursor", bg = colors.super_white, fg = "#282c34" }
hl { "CursorLine", bg = "#333842" }
hl { "PMenu", bg = "#373b41", fg = "#b4b7b4" }
hl { "PMenuSel", bg = "#fbffad", fg = "#282c34" }
hl { "PMenuSbar", bg = "#282c34" }
hl { "PMenuThumb", bg = "#b4b7b4" }
hl { "Search", bg = colors.yellow, fg = "#282a2e" }
hl { "TabLine", bg = "#282a2e", fg = "#5f89ad" }
hl { "TabLineFill", bg = "#969896", fg = "#ebdbb2" }
hl { "TabLineSel", bg = "#282a2e", fg = "#ffffff" }
hl { "ColorColumn", bg = "#81a2be", fg = "#ffffff" }
hl { "qfFileName", fg = colors.yellow }

hl { "Special", fg = "#aa92cd", styles = { styles.italic } }

hl { "SpecialChar", fg = "#a3685a" }
hl { "NonText", fg = "#4e545c" }
hl { "WhiteSpace", fg = "#8e6fbd" }
hl { "Conceal", bg = "#4e545c", fg = "#282c34" }

hl { "StatusLine", bg = "#81a2be", fg = "#373b41" }

hl { "StatusLineNC", bg = "#3f4349", fg = "#969896" }

hl { "User1", bg = colors.yellow, fg = "#ffffff" }
hl { "User2", bg = colors.red, fg = "#ffffff" }
hl { "User3", bg = colors.green, fg = "#ffffff" }
hl { "CommandMode", link = "Constant" }
hl { "VisualMode", link = "StatusLine" }
hl { "NormalMode", link = "Function" }
hl { "InsertMode", link = "StatusLine" }
hl { "TerminalMode", link = "Constant" }

-- Booleans
hl { "Boolean", fg = "#7fbfff" }
hl { "TSBoolean", link = "Boolean" }

hl { "Error", fg = "#d98c8c" }
hl { { "Label", "TSLabel" }, fg = colors.red }
hl { { "Operator", "TSOperator" }, fg = "#e6b3b3" }
hl { "PreProc", fg = colors.yellow }
hl { { "Repeat", "TSRepeat" }, fg = colors.red }
hl { "Repeat", fg = colors.red }
hl { "Statement", fg = "#c04040" }
hl { "StorageClass", fg = colors.yellow }
hl { { "String", "TSString" }, fg = colors.green }
hl { "Structure", fg = "#b294bb" }
hl { { "Tag", "TSTag" }, fg = colors.yellow }
hl { "Todo", fg = colors.yellow }
hl { "Typedef", fg = colors.yellow }
hl { { "Type", "TSType" }, fg = "#b294bb" }
hl { "Folded", bg = "#4e545c", fg = "#7c7f7c" }
hl { { "vimFunction", "Function", "TSFunction", "pythonBuiltinFunc" }, fg = colors.yellow }

hl { { "Number", "Float", "TSNumber", "TSFloat" }, fg = colors.red }
hl { "MatchParen", styles = { styles.underline } }
hl { { "TSCharacter", "Character" }, fg = colors.red }
hl { { "TSComment", "Comment" }, fg = "#969896", styles = { styles.italic } }
hl { { "Conditional", "TSConditional" }, fg = "#f8fe8a", styles = { styles.bold } }
hl { { "Identifier", "TSConstant", "Constant" }, fg = colors.red }

hl { { "Define", "TSInclude", "Include" }, fg = "#8abeb7" }
hl { { "vimCommand", "vimNotFunc", "TSConstant", "vimLet" }, fg = "#81a2be" }
hl { "TSVariable", fg = colors.super_white }
hl { { "TSVariableBuiltin", "TSConstantBuiltin" }, fg = colors.yellow }
hl {
  { "vimIsCommand", "vimFuncVar", "TSKeyword", "Keyword" },
  fg = "#b294bb",
  styles = { styles.bold },
}
hl { "TSLabel", fg = colors.red }

hl { "HelpDoc", bg = colors.dark_green, fg = "#ffffff" }
hl { "HelpIgnore", fg = colors.green }

hl { "gitDiff", fg = "#c7c7c7" }
hl { "DiffChange", bg = "#2800" }
hl { "DiffText", bg = "#8e00" }
hl { "DiffAdd", bg = "#2800" }
hl { "DiffDelete", bg = "#0" }
hl { "DiffRemoved", fg = colors.red }
hl { "DiffAdded", fg = colors.green }

hl { "VimwikiBold", fg = colors.red }
hl { "TelescopeMatching", fg = "#f2904b", styles = { styles.bold } }

hl { "StartifyBracket", fg = colors.red }
hl { "StartifyFile", fg = "#c04040" }
hl { "StartifyNumber", fg = "#81a2be" }
hl { "StartifyPath", fg = "#77bb77" }
hl { "StartifySlash", fg = "#8abeb7" }
hl { "StartifySection", fg = "#fbffad" }
hl { "StartifySpecial", fg = "#de935f" }
hl { "StartifyHeader", fg = "#de935f" }
hl { "StartifyFooter", fg = "#373b41" }

hl { "foldbraces", fg = "#f2e5bc" }

hl { "markdownH1", fg = "#81a2be" }
hl { "markdownH2", fg = "#a3bbd0" }
hl { "markdownH3", fg = "#c5d4e1" }

hl { "vimCommentTitle", fg = colors.red }
hl { "vimMapModKey", fg = "#8abeb7" }
hl { "vimNotation", fg = "#8abeb7" }
hl { "vimMapLHS", fg = colors.yellow }
hl { "vimNotation", fg = "#8abeb7" }
hl { "vimBracket", fg = "#96535c" }
hl { "vimMap", fg = colors.dark_green }
hl { "nvimMap", fg = colors.dark_green }
hl { "vimAutoloadFunction", fg = "#f3fe14" }

-- LSP
hl { { "LspReferenceRead", "LspReferenceWrite" }, bg = "#414f5e" }
-- hl { 'LspReferenceText'    , bg='#333842' }

-- Git signs
hl { "GitSignsAddNr", bg = "green" }
