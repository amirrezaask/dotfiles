-- vercel Colorscheme for Neovim
-- Adapts to vim.o.background (dark / light) — palette from gantoreno/vscode-vercel

local transparent = vim.g.transparency

local palettes = {
	dark = {
		bg = "#000000",
		fg = "#ededed",
		muted = "#a1a1a1",
		dim = "#878787",
		subtle = "#676767",
		border = "#333333",
		surface = "#0a0a0a",
		surface_alt = "#242424",
		pink = "#f05b8d",
		green = "#58c760",
		orange = "#f99902",
		blue = "#62a6ff",
		purple = "#b675f1",
		string = "#58c760",
		string_alt = "#62a6ff",
		red = "#f56464",
		cursor = "#ededed",
		cursorline = "#1a1a1a",
		selection = "#333333",
		codeblock = "#0a0a0a",
		inactive = "#242424",
		h1_bg = "#3d1f2a",
		h2_bg = "#2a1f3d",
		h3_bg = "#1f2a3d",
		h4_bg = "#1f3d2a",
		h5_bg = "#3d2a1f",
		h6_bg = "#2a2a2a",
		heading_fg = "#000000",
		on_accent = "#000000",
	},
	light = {
		bg = "#ffffff",
		fg = "#171717",
		muted = "#666666",
		dim = "#a8a8a8",
		subtle = "#bbbbbb",
		border = "#cccccc",
		surface = "#fafafa",
		surface_alt = "#ebebeb",
		pink = "#b32c62",
		green = "#397c3b",
		orange = "#9e5200",
		blue = "#005ee9",
		purple = "#7200c4",
		string = "#397c3b",
		string_alt = "#005ee9",
		red = "#c62128",
		cursor = "#171717",
		cursorline = "#f5f5f5",
		selection = "#cccccc",
		codeblock = "#fafafa",
		inactive = "#ebebeb",
		h1_bg = "#ffebe9",
		h2_bg = "#fbefff",
		h3_bg = "#ddf4ff",
		h4_bg = "#dafbe1",
		h5_bg = "#fff8c5",
		h6_bg = "#ebebeb",
		heading_fg = "#fafafa",
		on_accent = "#fafafa",
	},
}

local function palette()
	return palettes[vim.o.background == "light" and "light" or "dark"]
end

local function apply()
	local c = palette()
	local bg = transparent and "NONE" or c.bg
	local float_bg = bg
	local cursor_fg = transparent and c.bg or c.bg

	local highlights = {
		Normal = { fg = c.fg, bg = bg },
		NormalFloat = { fg = c.fg, bg = float_bg },
		NormalNC = { fg = c.muted, bg = bg },
		Cursor = { bg = c.cursor, fg = cursor_fg },
		CursorLine = { bg = c.cursorline },
		CursorLineNr = { fg = c.muted, bold = true },
		Visual = { bg = c.selection },
		VisualNOS = { bg = c.selection },
		LineNr = { fg = c.subtle },
		LineNrAbove = { fg = c.dim },
		LineNrBelow = { fg = c.dim },
		SignColumn = { fg = c.subtle, bg = bg },
		Folded = { fg = c.purple, bg = c.surface },
		FoldColumn = { fg = c.muted },
		Comment = { fg = c.muted, italic = true },
		SpecialComment = { fg = c.muted, italic = true },

		Keyword = { fg = c.pink },
		Function = { fg = c.purple },
		String = { fg = c.string },
		Character = { fg = c.string },
		Type = { fg = c.blue },
		Typedef = { fg = c.blue },
		Variable = { fg = c.fg },
		Identifier = { fg = c.fg },
		Constant = { fg = c.blue },
		Number = { fg = c.blue },
		Float = { fg = c.blue },
		Boolean = { fg = c.blue },
		Operator = { fg = c.fg },
		Delimiter = { fg = c.fg },
		PreProc = { fg = c.pink },
		Include = { fg = c.pink },
		Define = { fg = c.pink },
		Macro = { fg = c.purple },
		Tag = { fg = c.green },
		Special = { fg = c.blue },
		SpecialChar = { fg = c.green },
		Title = { fg = c.fg, bold = true },
		Underlined = { fg = c.blue, underline = true },
		Error = { fg = c.red, bg = c.surface },
		Todo = { fg = c.orange, bold = true },

		["@keyword"] = { fg = c.pink },
		["@keyword.function"] = { fg = c.pink },
		["@keyword.import"] = { fg = c.pink },
		["@keyword.export"] = { fg = c.pink },
		["@keyword.return"] = { fg = c.pink },
		["@keyword.operator"] = { fg = c.pink },

		["@function"] = { fg = c.purple },
		["@function.builtin"] = { fg = c.purple },
		["@function.call"] = { fg = c.purple },
		["@function.method"] = { fg = c.purple },
		["@function.method.call"] = { fg = c.purple },
		["@method"] = { fg = c.purple },
		["@method.call"] = { fg = c.purple },

		["@string"] = { fg = c.string },
		["@string.escape"] = { fg = c.string_alt },
		["@string.regex"] = { fg = c.string_alt },
		["@string.special"] = { fg = c.string },
		["@string.regexp"] = { fg = c.string_alt },

		["@number"] = { fg = c.blue },
		["@float"] = { fg = c.blue },
		["@boolean"] = { fg = c.blue },

		["@constant"] = { fg = c.blue },
		["@constant.builtin"] = { fg = c.blue },
		["@constant.macro"] = { fg = c.purple },

		["@type"] = { fg = c.blue },
		["@type.builtin"] = { fg = c.blue },
		["@type.definition"] = { fg = c.blue },

		["@variable"] = { fg = c.fg },
		["@variable.builtin"] = { fg = c.blue },
		["@variable.parameter"] = { fg = c.fg },
		["@variable.member"] = { fg = c.blue },
		["@property"] = { fg = c.blue },
		["@field"] = { fg = c.blue },

		["@operator"] = { fg = c.fg },
		["@punctuation"] = { fg = c.fg },
		["@punctuation.bracket"] = { fg = c.fg },
		["@punctuation.delimiter"] = { fg = c.fg },
		["@punctuation.special"] = { fg = c.pink },

		["@comment"] = { fg = c.muted, italic = true },
		["@comment.documentation"] = { fg = c.muted, italic = true },

		["@tag"] = { fg = c.green },
		["@tag.builtin"] = { fg = c.green },
		["@tag.attribute"] = { fg = c.purple },
		["@tag.delimiter"] = { fg = c.fg },
		["@attribute"] = { fg = c.purple },

		["@constructor"] = { fg = c.blue },
		["@module"] = { fg = c.fg },
		["@namespace"] = { fg = c.blue },
		["@include"] = { fg = c.pink },

		["@label"] = { fg = c.orange },
		["@text"] = { fg = c.fg },
		["@text.literal"] = { fg = c.fg },
		["@text.title"] = { fg = c.fg, bold = true },
		["@text.reference"] = { fg = c.blue },
		["@text.uri"] = { fg = c.string_alt, underline = true },
		["@text.emphasis"] = { fg = c.fg, italic = true },
		["@text.strong"] = { fg = c.fg, bold = true },

		["@markup.heading"] = { fg = c.blue, bold = true },
		["@markup.link"] = { fg = c.string_alt, underline = true },
		["@markup.link.url"] = { fg = c.string_alt, underline = true },
		["@markup.raw"] = { fg = c.blue },
		["@markup.italic"] = { fg = c.fg, italic = true },
		["@markup.bold"] = { fg = c.fg, bold = true },
		["@markup.list"] = { fg = c.orange },

		["@parameter"] = { fg = c.fg },
		["@symbol"] = { fg = c.blue },
		["@special"] = { fg = c.blue },

		DiagnosticError = { fg = c.pink },
		DiagnosticWarn = { fg = c.orange },
		DiagnosticInfo = { fg = c.blue },
		DiagnosticHint = { fg = c.muted },
		DiagnosticUnderlineError = { sp = c.pink, undercurl = true },
		DiagnosticUnderlineWarn = { sp = c.orange, undercurl = true },
		DiagnosticUnderlineInfo = { sp = c.blue, undercurl = true },
		DiagnosticUnderlineHint = { sp = c.muted, undercurl = true },
		DiagnosticVirtualTextError = { bg = c.pink, fg = c.on_accent },
		DiagnosticVirtualTextWarn = { bg = c.orange, fg = c.on_accent },
		DiagnosticVirtualTextInfo = { bg = c.blue, fg = c.on_accent },
		DiagnosticVirtualTextHint = { bg = c.subtle, fg = c.fg },

		DiffAdd = { bg = c.surface, fg = c.green },
		DiffChange = { bg = c.surface, fg = c.orange },
		DiffDelete = { bg = c.surface, fg = c.pink },
		DiffText = { bg = c.border, fg = c.fg },

		Headline1 = { fg = c.pink, bg = c.h1_bg, bold = true },
		Headline2 = { fg = c.purple, bg = c.h2_bg, bold = true },
		Headline3 = { fg = c.blue, bg = c.h3_bg, bold = true },
		Headline4 = { fg = c.green, bg = c.h4_bg, bold = true },
		Headline5 = { fg = c.orange, bg = c.h5_bg, bold = true },
		Headline6 = { fg = c.fg, bg = c.h6_bg, bold = true },

		RenderMarkdownCode = { bg = c.codeblock },
		RenderMarkdownCodeInline = { fg = c.blue, bg = c.surface },

		FloatBorder = { fg = c.border, bg = float_bg },
		FloatTitle = { fg = c.purple, bg = float_bg, bold = true },
		Pmenu = { fg = c.fg, bg = float_bg },
		PmenuSel = { fg = c.heading_fg, bg = c.purple },
		PmenuSbar = { bg = float_bg },
		PmenuThumb = { bg = c.subtle },
		MatchParen = { fg = c.pink, bold = true },
		Search = { fg = c.heading_fg, bg = c.orange },
		IncSearch = { fg = c.on_accent, bg = c.pink },
		Substitute = { fg = c.on_accent, bg = c.purple },
		WinSeparator = { fg = c.border },
		StatusLine = { bg = "NONE" },
		StatusLineNC = { bg = "NONE" },
		WinBar = { fg = c.muted, bg = "NONE" },
		WinBarNC = { fg = c.dim, bg = "NONE" },

		TelescopeNormal = { fg = c.fg, bg = float_bg },
		TelescopeSelection = { fg = c.heading_fg, bg = c.purple },
		TelescopeMultiSelection = { fg = c.blue, bg = float_bg },

		GitSignsAdd = { fg = c.green },
		GitSignsChange = { fg = c.orange },
		GitSignsDelete = { fg = c.pink },

		MiniDiffSignAdd = { fg = c.green, bold = true },
		MiniDiffSignChange = { fg = c.orange, bold = true },
		MiniDiffOverAdd = { bg = c.h4_bg, fg = c.green },
		MiniDiffOverDelete = { bg = c.h1_bg, fg = c.pink },
		MiniDiffOverChange = { bg = c.h3_bg, fg = c.blue },

		SpellBad = { sp = c.pink, undercurl = true },
		SpellCap = { sp = c.orange, undercurl = true },
		SpellLocal = { sp = c.blue, undercurl = true },
		SpellRare = { sp = c.purple, undercurl = true },

		BlinkCmpMenu = { fg = c.fg, bg = float_bg },
		BlinkCmpMenuBorder = { fg = c.border, bg = float_bg },
		BlinkCmpMenuSelection = { fg = c.heading_fg, bg = c.purple },
		BlinkCmpDoc = { fg = c.fg, bg = float_bg },
		BlinkCmpDocBorder = { fg = c.border, bg = float_bg },
		BlinkCmpGhostText = { fg = c.muted, italic = true },

		SnacksNormal = { fg = c.fg, bg = float_bg },
		SnacksWin = { fg = c.fg, bg = float_bg },
		SnacksInputNormal = { fg = c.fg, bg = float_bg },
		SnacksInputBorder = { fg = c.border, bg = float_bg },
		SnacksInputTitle = { fg = c.purple, bg = float_bg, bold = true },
		SnacksPickerNormal = { fg = c.fg, bg = float_bg },
		SnacksPickerBorder = { fg = c.border, bg = float_bg },
		SnacksPickerTitle = { fg = c.purple, bg = float_bg, bold = true },
		SnacksPickerList = { fg = c.fg, bg = float_bg },
		SnacksPickerSelected = { fg = c.heading_fg, bg = c.purple },
		SnacksPickerDir = { fg = c.blue },
		SnacksPickerMatch = { fg = c.pink, bold = true },

		OilFloat = { fg = c.fg, bg = float_bg },
		OilBorder = { fg = c.border, bg = float_bg },

		TreesitterContext = { fg = c.muted, bg = c.surface },
		TreesitterContextBottom = { fg = c.border, bg = c.surface },
	}

	for group, opts in pairs(highlights) do
		vim.api.nvim_set_hl(0, group, opts)
	end
end

if not vim.g.vercel_background_autocmd then
	vim.g.vercel_background_autocmd = true
	vim.api.nvim_create_autocmd("OptionSet", {
		pattern = "background",
		callback = function()
			if vim.g.colors_name == "vercel" then
				apply()
			end
		end,
	})
end

apply()
vim.g.colors_name = "vercel"
