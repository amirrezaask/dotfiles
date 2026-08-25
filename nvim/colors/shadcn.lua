-- shadcn/ui neutral dark colorscheme for Neovim
-- Source: https://ui.shadcn.com/docs/theming
-- Neutral surfaces follow shadcn's default tokens. Syntax accents use its
-- chart/destructive hues, lifted only where terminal text needs more contrast.

local transparent = vim.g.transparency == true or vim.g.transparency == 1

vim.o.background = "dark"
vim.cmd("highlight clear")
if vim.fn.exists("syntax_on") == 1 then vim.cmd("syntax reset") end

local c = {
 -- shadcn neutral dark roles
 bg = "#0a0a0a", -- background
 fg = "#d4d4d4", -- foreground
 fg_dim = "#a1a1a1", -- muted-foreground
 cursor = "#e5e5e5", -- primary
 surface = "#171717", -- card / popover
 surface_alt = "#262626", -- secondary / muted / accent
 selection = "#404040", -- neutral-700; clearer than accent on the base canvas
 border = "#525252", -- neutral-600; visible component boundary
 separator = "#404040", -- neutral-700; quiet structural divider
 ring = "#737373", -- ring

 -- shadcn chart and semantic roles
 builtin = "#60a5fa", -- chart-1, lifted for dark-background text contrast
 constant = "#00bc7d", -- chart-2
 type = "#fe9a00", -- chart-3
 keyword = "#c084fc", -- chart-4, lifted for popover text contrast
 number = "#ff2056", -- chart-5
 destructive = "#ff6467",
 macro = "#2dd4bf", -- cyan companion for hints and macros
 warning = "#fe9a00",

 -- Low-chroma semantic surfaces keep diagnostics readable without neon blocks.
 error_bg = "#321719",
 warning_bg = "#30220b",
 success_bg = "#0c2a20",
 info_bg = "#13253a",

 operator = "#a1a1a1",
 punctuation = "#8a8a8a",
 variable = "#d4d4d4",
 string = "#00bc7d",
 comment = "#a1a1a1",
 function_name = "#60a5fa",
 preprocessor = "#c084fc",
 region = "#404040",
 cursorline = "#171717",
 mode_line_fg = "#e5e5e5",
 mode_line_bg = "#262626",
 paren_match_bg = "#e5e5e5",
 paren_match_fg = "#171717",
 search_bg = "#fe9a00",
 search_fg = "#171717",
 muted = "#a1a1a1",
 dim = "#8a8a8a",
 ghost = "#525252",
}

local function apply()
 local bg = transparent and "NONE" or c.bg
 -- Popovers stay opaque even when the editor canvas is transparent so that
 -- completion and picker text never competes with content underneath.
 local float_bg = c.surface
 local cursor_fg = c.bg

 local highlights = {
  Normal = { fg = c.fg, bg = bg },
  NormalFloat = { fg = c.fg, bg = float_bg },
  NormalNC = { fg = c.fg_dim, bg = bg },
  Cursor = { bg = c.cursor, fg = cursor_fg },
  CursorLine = { bg = c.cursorline },
  CursorLineNr = { fg = c.mode_line_fg, bold = true, bg = c.cursorline },
  CursorLineFold = { bg = c.cursorline },
  CursorLineSign = { bg = c.cursorline },
  CursorColumn = { bg = c.cursorline },
  Visual = { fg = c.fg, bg = c.region },
  VisualNOS = { fg = c.fg, bg = c.region },
  LineNr = { fg = c.dim, bg = bg },
  LineNrAbove = { fg = c.dim, bg = bg },
  LineNrBelow = { fg = c.dim, bg = bg },
  SignColumn = { fg = c.dim, bg = bg },
  Folded = { fg = c.type, bg = c.surface },
  FoldColumn = { fg = c.muted },
  Comment = { fg = c.comment },
  SpecialComment = { fg = c.comment },

  Keyword = { fg = c.keyword },
  Function = { fg = c.function_name },
  String = { fg = c.string },
  Character = { fg = c.string },
  Type = { fg = c.type },
  Typedef = { fg = c.type },
  Variable = { fg = c.variable },
  Identifier = { fg = c.variable },
  Constant = { fg = c.constant },
  Number = { fg = c.number },
  Float = { fg = c.number },
  Boolean = { fg = c.constant },
  Operator = { fg = c.operator },
  Delimiter = { fg = c.punctuation },
  PreProc = { fg = c.preprocessor },
  Include = { fg = c.preprocessor },
  Define = { fg = c.macro },
  Macro = { fg = c.macro },
  Tag = { fg = c.type },
  Special = { fg = c.macro },
  SpecialChar = { fg = c.string },
  Title = { fg = c.fg, bold = true },
  Underlined = { fg = c.macro, underline = true },
  Error = { fg = c.destructive, bg = c.error_bg },
  ErrorMsg = { fg = c.destructive, bold = true },
  WarningMsg = { fg = c.warning },
  Directory = { fg = c.type },
  Added = { fg = c.constant },
  Changed = { fg = c.warning },
  Removed = { fg = c.destructive },
  Todo = { fg = c.destructive, bold = true },

  ["@keyword"] = { fg = c.keyword },
  ["@keyword.function"] = { fg = c.keyword },
  ["@keyword.import"] = { fg = c.keyword },
  ["@keyword.export"] = { fg = c.keyword },
  ["@keyword.return"] = { fg = c.keyword },
  ["@keyword.operator"] = { fg = c.keyword },
  ["@keyword.conditional"] = { fg = c.keyword },
  ["@keyword.repeat"] = { fg = c.keyword },
  ["@keyword.exception"] = { fg = c.keyword },

  ["@function"] = { fg = c.function_name },
  ["@function.builtin"] = { fg = c.function_name },
  ["@function.call"] = { fg = c.function_name },
  ["@function.method"] = { fg = c.function_name },
  ["@function.method.call"] = { fg = c.function_name },
  ["@method"] = { fg = c.function_name },
  ["@method.call"] = { fg = c.function_name },

  ["@string"] = { fg = c.string },
  ["@string.escape"] = { fg = c.number },
  ["@string.regex"] = { fg = c.macro },
  ["@string.special"] = { fg = c.string },
  ["@string.regexp"] = { fg = c.macro },
  ["@string.documentation"] = { fg = c.string },

  ["@number"] = { fg = c.number },
  ["@float"] = { fg = c.number },
  ["@boolean"] = { fg = c.constant },

  ["@constant"] = { fg = c.constant },
  ["@constant.builtin"] = { fg = c.macro },
  ["@constant.macro"] = { fg = c.macro },

  ["@type"] = { fg = c.type },
  ["@type.builtin"] = { fg = c.keyword },
  ["@type.qualifier"] = { fg = c.keyword },
  ["@type.definition"] = { fg = c.type },

  ["@variable"] = { fg = c.variable },
  ["@variable.builtin"] = { fg = c.builtin },
  ["@variable.parameter"] = { fg = c.variable },
  ["@variable.member"] = { fg = c.variable },
  ["@property"] = { fg = c.variable },
  ["@field"] = { fg = c.variable },

  ["@operator"] = { fg = c.operator },
  ["@punctuation"] = { fg = c.punctuation },
  ["@punctuation.bracket"] = { fg = c.punctuation },
  ["@punctuation.delimiter"] = { fg = c.punctuation },
  ["@punctuation.special"] = { fg = c.keyword },

  ["@comment"] = { fg = c.comment },
  ["@comment.documentation"] = { fg = c.comment },
  ["@comment.todo"] = { fg = c.destructive, bold = true },

  ["@tag"] = { fg = c.type },
  ["@tag.builtin"] = { fg = c.macro },
  ["@tag.attribute"] = { fg = c.variable },
  ["@tag.delimiter"] = { fg = c.punctuation },
  ["@attribute"] = { fg = c.variable },

  ["@constructor"] = { fg = c.type },
  ["@module"] = { fg = c.fg },
  ["@namespace"] = { fg = c.macro },
  ["@include"] = { fg = c.preprocessor },

  ["@label"] = { fg = c.mode_line_fg },
  ["@text"] = { fg = c.fg },
  ["@text.literal"] = { fg = c.fg },
  ["@text.title"] = { fg = c.fg, bold = true },
  ["@text.reference"] = { fg = c.type },
  ["@text.uri"] = { fg = c.constant, underline = true },
  ["@text.emphasis"] = { fg = c.fg, italic = true },
  ["@text.strong"] = { fg = c.fg, bold = true },

  ["@markup.heading"] = { fg = c.type, bold = true },
  ["@markup.link"] = { fg = c.constant, underline = true },
  ["@markup.link.url"] = { fg = c.constant, underline = true },
  ["@markup.raw"] = { fg = c.constant },
  ["@markup.italic"] = { fg = c.fg, italic = true },
  ["@markup.bold"] = { fg = c.fg, bold = true },
  ["@markup.list"] = { fg = c.mode_line_fg },

  ["@parameter"] = { fg = c.variable },
  ["@symbol"] = { fg = c.macro },
  ["@special"] = { fg = c.macro },

  DiagnosticError = { fg = c.destructive },
  DiagnosticWarn = { fg = c.warning },
  DiagnosticInfo = { fg = c.builtin },
  DiagnosticHint = { fg = c.macro },
  DiagnosticOk = { fg = c.constant },
  DiagnosticUnderlineError = { sp = c.destructive, undercurl = true },
  DiagnosticUnderlineWarn = { sp = c.warning, undercurl = true },
  DiagnosticUnderlineInfo = { sp = c.builtin, undercurl = true },
  DiagnosticUnderlineHint = { sp = c.macro, undercurl = true },
  DiagnosticUnderlineOk = { sp = c.constant, undercurl = true },
  DiagnosticVirtualTextError = { bg = c.error_bg, fg = c.destructive },
  DiagnosticVirtualTextWarn = { bg = c.warning_bg, fg = c.warning },
  DiagnosticVirtualTextInfo = { bg = c.info_bg, fg = c.builtin },
  DiagnosticVirtualTextHint = { bg = c.success_bg, fg = c.macro },
  DiagnosticVirtualTextOk = { bg = c.success_bg, fg = c.constant },

  DiffAdd = { bg = c.success_bg, fg = c.constant },
  DiffChange = { bg = c.warning_bg, fg = c.warning },
  DiffDelete = { bg = c.error_bg, fg = c.destructive },
  DiffText = { bg = c.info_bg, fg = c.builtin },

  Headline1 = { fg = c.function_name, bg = c.surface, bold = true },
  Headline2 = { fg = c.type, bg = c.surface_alt, bold = true },
  Headline3 = { fg = c.constant, bg = c.success_bg, bold = true },
  Headline4 = { fg = c.keyword, bg = c.surface, bold = true },
  Headline5 = { fg = c.mode_line_fg, bg = c.surface_alt, bold = true },
  Headline6 = { fg = c.fg, bg = c.surface, bold = true },

  RenderMarkdownCode = { bg = c.surface },
  RenderMarkdownCodeInline = { fg = c.constant, bg = c.surface },

  FloatBorder = { fg = c.border, bg = float_bg },
  FloatTitle = { fg = c.mode_line_fg, bg = float_bg, bold = true },
  FloatFooter = { fg = c.muted, bg = float_bg },
  Pmenu = { fg = c.fg, bg = float_bg },
  PmenuSel = { fg = c.paren_match_fg, bg = c.paren_match_bg, bold = true },
  PmenuSbar = { bg = float_bg },
  PmenuThumb = { bg = c.ring },
  PmenuMatch = { fg = c.keyword, bold = true },
  PmenuMatchSel = { fg = c.paren_match_fg, bg = c.paren_match_bg, bold = true },
  PmenuKind = { fg = c.type },
  PmenuKindSel = { fg = c.paren_match_fg, bg = c.paren_match_bg },
  PmenuExtra = { fg = c.muted },
  PmenuExtraSel = { fg = c.paren_match_fg, bg = c.paren_match_bg },
  MatchParen = { fg = c.paren_match_fg, bg = c.paren_match_bg, bold = true },
  Search = { fg = c.search_fg, bg = c.search_bg },
  IncSearch = { fg = c.paren_match_fg, bg = c.paren_match_bg, bold = true },
  Substitute = { fg = c.paren_match_fg, bg = c.destructive, bold = true },
  WinSeparator = { fg = c.separator },
  StatusLine = { fg = c.mode_line_fg, bg = c.mode_line_bg, bold = true },
  StatusLineNC = { fg = c.muted, bg = c.surface },
  WinBar = { fg = c.mode_line_fg, bg = "NONE" },
  WinBarNC = { fg = c.dim, bg = "NONE" },
  TabLine = { fg = c.muted, bg = c.surface },
  TabLineFill = { bg = c.surface },
  TabLineSel = { fg = c.fg, bg = c.selection, bold = true },

  MsgArea = { fg = c.fg, bg = bg },
  MsgSeparator = { fg = c.separator, bg = bg },
  MoreMsg = { fg = c.constant },
  Question = { fg = c.constant },
  ModeMsg = { fg = c.mode_line_fg },
  EndOfBuffer = { fg = c.ghost, bg = bg },
  NonText = { fg = c.ghost },
  Whitespace = { fg = c.ghost },
  Conceal = { fg = c.ghost },
  QuickFixLine = { fg = c.paren_match_fg, bg = c.paren_match_bg, bold = true },
  CurSearch = { fg = c.paren_match_fg, bg = c.paren_match_bg, bold = true },

  TelescopeNormal = { fg = c.fg, bg = float_bg },
  TelescopeBorder = { fg = c.border, bg = float_bg },
  TelescopePromptNormal = { fg = c.fg, bg = float_bg },
  TelescopePromptBorder = { fg = c.ring, bg = float_bg },
  TelescopePromptTitle = { fg = c.mode_line_fg, bg = float_bg, bold = true },
  TelescopeSelection = { fg = c.paren_match_fg, bg = c.paren_match_bg },
  TelescopeMultiSelection = { fg = c.type, bg = float_bg },
  TelescopeResultsTitle = { fg = c.mode_line_fg, bg = float_bg, bold = true },

  FzfLuaNormal = { fg = c.fg, bg = float_bg },
  FzfLuaBorder = { fg = c.border, bg = float_bg },
  FzfLuaTitle = { fg = c.mode_line_fg, bg = float_bg, bold = true },
  FzfLuaCursor = { fg = c.paren_match_fg, bg = c.paren_match_bg },
  FzfLuaCursorLine = { bg = c.cursorline },
  FzfLuaDirPart = { fg = c.type },
  FzfLuaFilePart = { fg = c.fg },
  FzfLuaHeader = { fg = c.mode_line_fg, bold = true },
  FzfLuaHeaderText = { fg = c.keyword },
  FzfLuaInfo = { fg = c.comment },
  FzfLuaLivePrompt = { fg = c.fg, bold = true },
  FzfLuaLiveSym = { fg = c.type },
  FzfLuaPath = { fg = c.constant },
  FzfLuaPathCol = { fg = c.comment },
  FzfLuaPathLineNr = { fg = c.comment },
  FzfLuaPreviewBorder = { fg = c.border, bg = float_bg },
  FzfLuaPreviewTitle = { fg = c.mode_line_fg, bg = float_bg, bold = true },
  FzfLuaPreviewNormal = { fg = c.fg, bg = float_bg },
  FzfLuaPrompt = { fg = c.fg, bold = true },
  FzfLuaPromptBorder = { fg = c.ring, bg = float_bg },
  FzfLuaPromptTitle = { fg = c.mode_line_fg, bg = float_bg, bold = true },
  FzfLuaSpinner = { fg = c.mode_line_fg },
  FzfLuaSpinnerInfo = { fg = c.constant },
  FzfLuaSpinnerTitle = { fg = c.mode_line_fg, bold = true },

  GitSignsAdd = { fg = c.constant },
  GitSignsChange = { fg = c.type },
  GitSignsDelete = { fg = c.destructive },
  GitSignsChangedelete = { fg = c.type },
  GitSignsTopdelete = { fg = c.destructive },
  GitSignsUntracked = { fg = c.comment },

  MiniDiffSignAdd = { fg = c.constant, bold = true },
  MiniDiffSignChange = { fg = c.type, bold = true },
  MiniDiffSignDelete = { fg = c.destructive, bold = true },
  MiniDiffOverAdd = { bg = c.success_bg, fg = c.constant },
  MiniDiffOverDelete = { bg = c.error_bg, fg = c.destructive },
  MiniDiffOverChange = { bg = c.warning_bg, fg = c.warning },

  SpellBad = { sp = c.destructive, undercurl = true },
  SpellCap = { sp = c.type, undercurl = true },
  SpellLocal = { sp = c.constant, undercurl = true },
  SpellRare = { sp = c.builtin, undercurl = true },

  BlinkCmpMenu = { fg = c.fg, bg = float_bg },
  BlinkCmpMenuBorder = { fg = c.border, bg = float_bg },
  BlinkCmpMenuSelection = { fg = c.paren_match_fg, bg = c.paren_match_bg, bold = true },
  BlinkCmpDoc = { fg = c.fg, bg = float_bg },
  BlinkCmpDocBorder = { fg = c.border, bg = float_bg },
  BlinkCmpGhostText = { fg = c.muted },
  BlinkCmpKind = { fg = c.type },
  BlinkCmpLabel = { fg = c.fg },
  BlinkCmpLabelMatch = { fg = c.keyword, bold = true },
  BlinkCmpLabelDescription = { fg = c.comment },
  BlinkCmpLabelDetail = { fg = c.muted },
  BlinkCmpSource = { fg = c.comment },
  CmpItemMenu = { fg = c.comment },

  SnacksNormal = { fg = c.fg, bg = float_bg },
  SnacksWin = { fg = c.fg, bg = float_bg },
  SnacksInputNormal = { fg = c.fg, bg = float_bg },
  SnacksInputBorder = { fg = c.ring, bg = float_bg },
  SnacksInputTitle = { fg = c.mode_line_fg, bg = float_bg, bold = true },
  SnacksPickerNormal = { fg = c.fg, bg = float_bg },
  SnacksPickerBorder = { fg = c.border, bg = float_bg },
  SnacksPickerTitle = { fg = c.mode_line_fg, bg = float_bg, bold = true },
  SnacksPickerList = { fg = c.fg, bg = float_bg },
  SnacksPickerSelected = { fg = c.paren_match_fg, bg = c.paren_match_bg },
  SnacksPickerDir = { fg = c.type },
  SnacksPickerMatch = { fg = c.keyword, bold = true },

  OilFloat = { fg = c.fg, bg = float_bg },
  OilBorder = { fg = c.border, bg = float_bg },
  OilDirHidden = { fg = c.dim },
  OilDirIcon = { fg = c.type },
  OilDir = { fg = c.type },
  OilFile = { fg = c.fg },
  OilHidden = { fg = c.dim },
  OilLink = { fg = c.constant, underline = true },
  OilLinkTarget = { fg = c.constant },
  OilLinkPath = { fg = c.comment },
  OilParent = { fg = c.mode_line_fg },
  OilSelected = { fg = c.paren_match_fg, bg = c.paren_match_bg },

  TreesitterContext = { fg = c.muted, bg = c.surface },
  TreesitterContextLineNumber = { fg = c.dim, bg = c.surface },
  TreesitterContextBottom = { fg = c.separator, bg = c.surface },

  LspReferenceText = { bg = c.surface_alt, underline = true, sp = c.ring },
  LspReferenceRead = { bg = c.surface_alt, underline = true, sp = c.ring },
  LspReferenceWrite = { bg = c.surface_alt, underline = true, sp = c.ring },
  LspSignatureActiveParameter = { fg = c.keyword, bold = true },
  LspCodeLens = { fg = c.muted },
  LspInlayHint = { fg = c.fg_dim, bg = c.surface },
  DiagnosticDeprecated = { strikethrough = true },

  TermCursor = { bg = c.cursor, fg = cursor_fg },
  TermCursorNC = { bg = c.dim, fg = cursor_fg },

  VertSplit = { fg = c.separator },
  ColorColumn = { bg = c.surface },
  Ignore = { fg = c.dim },
  SpecialKey = { fg = c.dim },
  Vert = { fg = c.separator },

  NeogitBranch = { fg = c.type },
  NeogitRemote = { fg = c.constant },
  NeogitHunkHeader = { fg = c.mode_line_fg, bg = c.surface },
  NeogitHunkHeaderHighlight = { fg = c.paren_match_fg, bg = c.paren_match_bg },
  NeogitDiffAdd = { fg = c.constant },
  NeogitDiffDelete = { fg = c.destructive },
  NeogitDiffContext = { fg = c.fg },
  NeogitPopupBranchName = { fg = c.type, bold = true },

  MasonHeader = { fg = c.mode_line_fg, bg = c.mode_line_bg, bold = true },
  MasonHighlight = { fg = c.constant },
  MasonHighlightBlock = { fg = c.paren_match_fg, bg = c.constant },
  MasonHighlightBlockBold = { fg = c.paren_match_fg, bg = c.constant, bold = true },
  MasonMuted = { fg = c.comment },
  MasonMutedBlock = { fg = c.fg, bg = c.surface },

  WhichKey = { fg = c.type },
  WhichKeyGroup = { fg = c.mode_line_fg },
  WhichKeyDesc = { fg = c.fg },
  WhichKeySeparator = { fg = c.punctuation },
  WhichKeyFloat = { fg = c.fg, bg = float_bg },
  WhichKeyBorder = { fg = c.border, bg = float_bg },
  WhichKeyTitle = { fg = c.mode_line_fg, bg = float_bg, bold = true },

  NotifyINFO = { fg = c.builtin },
  NotifyWARN = { fg = c.warning },
  NotifyERROR = { fg = c.destructive },
  NotifyDEBUG = { fg = c.comment },
  NotifyTRACE = { fg = c.dim },
  NotifyBackground = { bg = float_bg },

  BufferLineFill = { bg = c.surface },
  BufferLineBackground = { fg = c.muted, bg = c.surface },
  BufferLineBufferVisible = { fg = c.fg, bg = c.surface_alt },
  BufferLineBufferSelected = { fg = c.fg, bg = c.selection, bold = true },
  BufferLineIndicatorSelected = { fg = c.builtin, bg = c.selection },
  BufferLineTab = { fg = c.muted, bg = c.surface },
  BufferLineTabSelected = { fg = c.fg, bg = c.selection, bold = true },
  BufferLineTabClose = { fg = c.destructive, bg = c.surface },

  NvimTreeNormal = { fg = c.fg, bg = bg },
  NvimTreeRootFolder = { fg = c.mode_line_fg, bold = true },
  NvimTreeFolderName = { fg = c.type },
  NvimTreeFolderIcon = { fg = c.type },
  NvimTreeEmptyFolderName = { fg = c.dim },
  NvimTreeIndentMarker = { fg = c.dim },
  NvimTreeGitDirty = { fg = c.type },
  NvimTreeGitStaged = { fg = c.constant },
  NvimTreeGitMerge = { fg = c.builtin },
  NvimTreeGitRenamed = { fg = c.constant },
  NvimTreeGitDeleted = { fg = c.destructive },
  NvimTreeGitNew = { fg = c.constant },

  HelpCommand = { fg = c.keyword },
  HelpExample = { fg = c.string },
  HelpHeader = { fg = c.mode_line_fg, bold = true },
  HelpHyperTextJump = { fg = c.constant, underline = true },
  HelpHyperTextEntry = { fg = c.constant },
  HelpKey = { fg = c.type },
  HelpSpecial = { fg = c.macro },
  HelpSectionDelim = { fg = c.separator },

  ManSubHeading = { fg = c.mode_line_fg, bold = true },
  ManOptionDesc = { fg = c.type },
  ManHeader = { fg = c.function_name, bold = true },

  healthSuccess = { fg = c.constant },
  healthWarning = { fg = c.type },
  healthError = { fg = c.destructive },
 }

 for group, opts in pairs(highlights) do
  vim.api.nvim_set_hl(0, group, opts)
 end
end

apply()
vim.g.colors_name = "shadcn"
