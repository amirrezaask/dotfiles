-- pikachu Colorscheme for Neovim
-- Auto-generated from linkarzu colorscheme
-- This colorscheme auto-applies when loaded via :colorscheme pikachu

local c = {
  bg = "#4a3f08",
  fg = "#ebfafa",
  purple = "#e19720",
  green = "#fad61d",
  cyan = "#fdf2bb",
  pink = "#fbe260",
  yellow = "#ebfafa",
  orange = "#811e09",
  lime = "#f62d14",
  comment = "#a5afc2",
  error = "#f62d14",
  warning = "#fad61d",
  selection = "#e9b3fd",
  cursor = "#F712FF",
  bg_light = "#3b3206",
  codeblock = "#6d5d0c",
  cursorline = "#a38a12",
  inactive = "#7f6c0e",
  h1_bg = "#855913",
  h2_bg = "#947e11",
  h3_bg = "#968f6f",
  h4_bg = "#956a39",
  h5_bg = "#8b9494",
  h6_bg = "#4c1205",
  heading_fg = "#4a3f08",
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
