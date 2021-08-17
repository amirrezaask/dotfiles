local hl = require("palette").highlight
local styles = require("palette").styles

local colors = {
  pure_black = "#000000",
  background = "#282c34",
  line_nr = "#282a2e",
  super_white = "#e0e0e0",
  white = "#c5c8c6",
  blue1 = "#81a2be",
  blue2 = "#5f89ad",
  blue3 = "#38556d",
  black = "#111317",
  yellow = "#f8fe7a",
  light_yellow = "#fbffad",
  red = "#cc6666",
  green = "#99cc99",
  dark_green = "#698b69",
  purple = "#8e6fbd",
  light_purple = "#aa92cd",
  white1 = "#ffffff",
  white2 = "#c7c7c7",
  white3 = "#b4b7b4",
  grey1 = "#969896",
  grey2 = "#4e545c",
  grey3 = "#333842",
  orange1 = "#f2904b",
  orange2 = "#de935f",
}

vim.g.colors_name = "gruvbuddy"
-- Extracted from https://github.com/tjdevries/gruvbuddy.nvim
hl { "Normal", bg = colors.background, fg = colors.white }
hl { "InvNormal", bg = colors.white, fg = colors.background }
hl { "NormalFloat", bg = colors.black, fg = colors.white1 }
hl { "LineNr", bg = colors.line_nr, fg = colors.grey1 }
hl { "EndOfBuffer", fg = colors.grey1 }
hl { "SignColumn", bg = colors.line_nr, fg = colors.grey1 }
hl { "Visual", bg = colors.blue3 }
hl { "VisualMode", bg = colors.blue3 }
hl { "VisualLineMode", bg = colors.blue3 }
hl { "Cursor", bg = colors.super_white, fg = colors.background }
hl { "CursorLine", bg = colors.grey3 }
hl { "PMenu", bg = colors.grey3, fg = colors.white2 }
hl { "PMenuSel", bg = colors.light_yellow, fg = colors.background }
hl { "PMenuSbar", bg = colors.background }
hl { "PMenuThumb", bg = colors.white2 }
hl { "Search", bg = colors.yellow, fg = colors.background }
hl { "TabLine", bg = colors.line_nr, fg = colors.blue1 }
hl { "TabLineFill", bg = colors.grey1, fg = "#ebdbb2" }
hl { "TabLineSel", bg = colors.line_nr, fg = colors.white1 }
hl { "ColorColumn", bg = colors.blue1, fg = colors.white1 }
hl { "qfFileName", fg = colors.yellow }

hl { "Special", fg = colors.light_purple, styles = { styles.italic } }

hl { "SpecialChar", fg = "#a3685a" }
hl { "NonText", fg = colors.grey2 }
hl { "WhiteSpace", fg = colors.purple }
hl { "Conceal", bg = colors.grey2, fg = colors.background }

hl { "StatusLine", bg = colors.blue1, fg = colors.grey3 }

hl { "StatusLineNC", bg = colors.grey3, fg = colors.grey1 }

hl { "User1", bg = colors.yellow, fg = colors.white1 }
hl { "User2", bg = colors.red, fg = colors.white1 }
hl { "User3", bg = colors.green, fg = colors.white1 }
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
hl { "Statement", fg = colors.red }
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
hl { { "TSComment", "Comment" }, fg = colors.grey1, styles = { styles.italic } }
hl { { "Conditional", "TSConditional" }, fg = colors.yellow, styles = { styles.bold } }
hl { { "Identifier", "TSConstant", "Constant" }, fg = colors.red }

hl { { "Define", "TSInclude", "Include" }, fg = "#8abeb7" }
hl { { "vimCommand", "vimNotFunc", "TSConstant", "vimLet" }, fg = colors.blue1 }
hl { "TSVariable", fg = colors.super_white }
hl { { "TSVariableBuiltin", "TSConstantBuiltin" }, fg = colors.yellow }
hl {
  { "vimIsCommand", "vimFuncVar", "TSKeyword", "Keyword" },
  fg = "#b294bb",
  styles = { styles.bold },
}
hl { "TSLabel", fg = colors.red }

hl { "HelpDoc", bg = colors.dark_green, fg = colors.white1 }
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
hl { "StartifyFile", fg = colors.red }
hl { "StartifyNumber", fg = colors.blue1 }
hl { "StartifyPath", fg = "#77bb77" }
hl { "StartifySlash", fg = "#8abeb7" }
hl { "StartifySection", fg = colors.light_yellow }
hl { "StartifySpecial", fg = colors.orange2 }
hl { "StartifyHeader", fg = colors.orange2 }
hl { "StartifyFooter", fg = "#373b41" }

hl { "foldbraces", fg = "#f2e5bc" }

hl { "markdownH1", fg = colors.blue1 }
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
