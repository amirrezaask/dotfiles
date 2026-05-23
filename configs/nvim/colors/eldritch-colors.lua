-- eldritch-colors Colorscheme for Neovim
-- Auto-generated from linkarzu colorscheme
-- This colorscheme auto-applies when loaded via :colorscheme eldritch-colors

local c = {
  bg = "#212337",
  fg = "#ebfafa",
  purple = "#a48cf2",
  green = "#37f499",
  cyan = "#04d1f9",
  pink = "#f265b5",
  yellow = "#f1fc79",
  orange = "#f16c75",
  lime = "#f7c67f",
  comment = "#a5afc2",
  error = "#f16c75",
  warning = "#f1fc79",
  selection = "#d99ffd",
  cursor = "#F712FF",
  bg_light = "#282b43",
  codeblock = "#314154",
  cursorline = "#314154",
  inactive = "#232e3b",
  h1_bg = "#625491",
  h2_bg = "#21925b",
  h3_bg = "#027d95",
  h4_bg = "#913c6d",
  h5_bg = "#909748",
  h6_bg = "#904146",
  heading_fg = "#212337",
}

-- Apply highlights
local highlights = {
  Normal = { fg = c.fg, bg = c.bg },
  NormalFloat = { bg = c.bg },
  Cursor = { bg = c.cursor },
  CursorLine = { bg = c.cursorline },
  Visual = { bg = c.selection, fg = c.bg },
  LineNr = { fg = c.selection },
  Comment = { fg = c.comment, italic = true },
  
  Keyword = { fg = c.purple },
  Function = { fg = c.green },
  String = { fg = c.cyan },
  Type = { fg = c.yellow },
  Variable = { fg = c.fg },
  Constant = { fg = c.pink },
  Number = { fg = c.orange },
  Operator = { fg = c.green },
  PreProc = { fg = c.lime },
  
  DiagnosticError = { fg = c.error },
  DiagnosticWarn = { fg = c.warning },
  DiagnosticInfo = { fg = c.cyan },
  DiagnosticHint = { fg = c.green },
  DiagnosticVirtualTextError = { bg = c.error, fg = c.bg },
  DiagnosticVirtualTextWarn = { bg = c.warning, fg = c.bg },
  
  DiffAdd = { bg = c.green, fg = c.bg },
  DiffChange = { bg = c.cyan, fg = c.bg },
  DiffDelete = { bg = c.error, fg = c.bg },
  
  Headline1 = { fg = c.purple, bg = c.h1_bg, bold = true },
  Headline2 = { fg = c.green, bg = c.h2_bg, bold = true },
  Headline3 = { fg = c.cyan, bg = c.h3_bg, bold = true },
  Headline4 = { fg = c.pink, bg = c.h4_bg, bold = true },
  Headline5 = { fg = c.yellow, bg = c.h5_bg, bold = true },
  Headline6 = { fg = c.orange, bg = c.h6_bg, bold = true },
  
  RenderMarkdownCode = { bg = c.codeblock },
  RenderMarkdownCodeInline = { fg = c.heading_fg, bg = c.green },
  
  TelescopeNormal = { fg = c.fg, bg = c.bg },
  TelescopeSelection = { fg = c.fg, bg = c.cursorline },
  TelescopeMultiSelection = { fg = c.green, bg = c.bg },
  
  FloatBorder = { fg = c.green, bg = c.bg },
  FloatTitle = { bg = c.bg },
  
  StatusLine = { bg = "NONE" },
  StatusLineNC = { bg = "NONE" },
  
  SpellBad = { sp = c.error, undercurl = true, bold = true, italic = true },
  SpellCap = { sp = c.warning, undercurl = true, bold = true, italic = true },
  
  MiniDiffSignAdd = { fg = c.yellow, bold = true },
  MiniDiffSignChange = { fg = c.green, bold = true },
  MiniDiffOverAdd = { bg = c.h2_bg, fg = c.fg },
  MiniDiffOverDelete = { bg = c.h4_bg, fg = c.fg },
  MiniDiffOverChange = { bg = c.h1_bg, fg = c.fg },
}

for group, opts in pairs(highlights) do
  vim.api.nvim_set_hl(0, group, opts)
end
