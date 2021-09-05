local hl = require("palette").highlight
local styles = require("palette").styles

vim.g.colors_name = "gruvbuddy"

-- Took and changed from https://github.com/tjdevries/gruvbuddy.nvim
local colors = {
  pure_black = "#000000",
  background = "#282c34",
  line_nr = "#282a2e",
  super_white = "#e0e0e0",
  white = "#c5c8c6",
  blue1 = "#7fbfff",
  -- blue1 = "#81a2be",
  blue2 = "#5f89ad",
  blue3 = "#38556d",
  black = "#111317",
  yellow1 = "#fbffad",
  yellow2 = "#f8fe7a",
  red1 = "#cc6666",
  red2 = "#96535c",
  green1 = "#8abeb7",
  green2 = "#77bb77",
  green3 = "#99cc99",
  dark_green = "#698b69",
  purple1 = "#aa92cd",
  purple2 = "#8e6fbd",
  white1 = "#ffffff",
  white2 = "#c7c7c7",
  white3 = "#b4b7b4",
  grey1 = "#969896",
  grey2 = "#4e545c",
  grey3 = "#333842",
  orange1 = "#f2904b",
  orange2 = "#de935f",
  pink1 = "#e6b3b3",
  pink2 = "#d98c8c",
  white_yellow = "#ebdbb2",
}

if vim.g.transparent == nil then
  hl { "Normal", bg = colors.background, fg = colors.white }
else
  hl { "Normal", bg = nil }
end
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
hl { "PMenuSel", bg = colors.yellow1, fg = colors.background }
hl { "PMenuSbar", bg = colors.background }
hl { "PMenuThumb", bg = colors.white2 }
hl { "Search", bg = colors.yellow2, fg = colors.background }
hl { "TabLine", bg = colors.line_nr, fg = colors.blue1 }
hl { "TabLineFill", bg = colors.grey1, fg = colors.white_yellow }
hl { "TabLineSel", bg = colors.line_nr, fg = colors.white1 }
hl { "ColorColumn", bg = colors.blue1, fg = colors.white1 }
hl { "qfFileName", fg = colors.yellow2 }

hl { "Special", fg = colors.purple1, styles = { styles.italic } }

hl { "SpecialChar", fg = colors.pink2 }
hl { "NonText", fg = colors.grey2 }
hl { "WhiteSpace", fg = colors.purple2 }
hl { "Conceal", bg = colors.grey2, fg = colors.background }

hl { "StatusLine", bg = colors.blue1, fg = colors.grey3 }

hl { "StatusLineNC", bg = colors.grey3, fg = colors.grey1 }

hl { "User1", bg = colors.yellow2, fg = colors.white1 }
hl { "User2", bg = colors.red1, fg = colors.white1 }
hl { "User3", bg = colors.green3, fg = colors.white1 }
hl { "CommandMode", link = "Constant" }
hl { "VisualMode", link = "StatusLine" }
hl { "NormalMode", link = "Function" }
hl { "InsertMode", link = "StatusLine" }
hl { "TerminalMode", link = "Constant" }

-- Booleans
hl { "Boolean", fg = colors.blue1 }
hl { "TSBoolean", link = "Boolean" }

hl { "Error", fg = colors.pink2 }
hl { { "Label", "TSLabel" }, fg = colors.red1 }
hl { { "Operator", "TSOperator" }, fg = colors.pink1 }
hl { "PreProc", fg = colors.yellow2 }
hl { { "Repeat", "TSRepeat" }, fg = colors.red1 }
hl { "Repeat", fg = colors.red1 }
hl { "Statement", fg = colors.red1 }
hl { "StorageClass", fg = colors.yellow2 }
hl { { "String", "TSString" }, fg = colors.green3 }
hl { "Structure", fg = colors.purple1 }
hl { { "Tag", "TSTag" }, fg = colors.yellow2 }
hl { "Todo", fg = colors.yellow2 }
hl { "Typedef", fg = colors.yellow2 }
hl { { "Type", "TSType" }, fg = colors.purple1 }
hl { "Folded", bg = colors.grey3, fg = colors.grey1 }
hl { { "vimFunction", "Function", "TSFunction", "pythonBuiltinFunc" }, fg = colors.yellow2 }

hl { { "Number", "Float", "TSNumber", "TSFloat" }, fg = colors.red1 }
hl { "MatchParen", styles = { styles.underline } }
hl { { "TSCharacter", "Character" }, fg = colors.red1 }
hl { { "TSComment", "Comment" }, fg = colors.grey1, styles = { styles.italic } }
hl { { "Conditional", "TSConditional" }, fg = colors.yellow2, styles = { styles.bold } }
hl { { "Identifier", "TSConstant", "Constant" }, fg = colors.red1 }

hl { { "Define", "TSInclude", "Include" }, fg = colors.green1 }
hl { { "vimCommand", "vimNotFunc", "TSConstant", "vimLet" }, fg = colors.blue1 }
hl { "TSVariable", fg = colors.super_white }
hl { { "TSVariableBuiltin", "TSConstantBuiltin" }, fg = colors.yellow2 }
hl {
  { "vimIsCommand", "vimFuncVar", "TSKeyword", "Keyword" },
  fg = colors.purple1,
  styles = { styles.bold },
}
hl { "TSLabel", fg = colors.red }

hl { "HelpDoc", bg = colors.green3, fg = colors.white1 }
hl { "HelpIgnore", fg = colors.green3 }

hl { "gitDiff", fg = colors.white2 }
hl { "DiffChange", bg = "#2800" }
hl { "DiffText", bg = "#8e00" }
hl { "DiffAdd", bg = "#2800" }
hl { "DiffDelete", bg = "#0" }
hl { "DiffRemoved", fg = colors.red }
hl { "DiffAdded", fg = colors.green3 }

hl { "VimwikiBold", fg = colors.red }
hl { "TelescopeMatching", fg = colors.orange1, styles = { styles.bold } }

hl { "StartifyBracket", fg = colors.red }
hl { "StartifyFile", fg = colors.red }
hl { "StartifyNumber", fg = colors.blue1 }
hl { "StartifyPath", fg = colors.green3 }
hl { "StartifySlash", fg = colors.green1 }
hl { "StartifySection", fg = colors.yellow1 }
hl { "StartifySpecial", fg = colors.orange2 }
hl { "StartifyHeader", fg = colors.orange2 }
hl { "StartifyFooter", fg = colors.grey3 }

hl { "foldbraces", fg = colors.white_yellow }

hl { "vimCommentTitle", fg = colors.red }
hl { "vimMapModKey", fg = colors.green1 }
hl { "vimNotation", fg = colors.green1 }
hl { "vimMapLHS", fg = colors.yellow2 }
hl { "vimNotation", fg = colors.green1 }
hl { "vimBracket", fg = colors.red2 }
hl { "vimMap", fg = colors.green3 }
hl { "nvimMap", fg = colors.green3 }
hl { "vimAutoloadFunction", fg = colors.yellow2 }

-- LSP
hl { { "LspReferenceRead", "LspReferenceWrite" }, bg = colors.blue3 }
-- hl { 'LspReferenceText'    , bg='#333842' }

-- Git signs
hl { "GitSignsAddNr", bg = colors.green2 }
