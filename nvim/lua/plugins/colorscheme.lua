return {
	{ "navarasu/onedark.nvim", opts = { style = "darker", transparent = TRANSPARENT } },
	{ "rose-pine/neovim", name = "rose-pine", opts = { disable_italics = true, disable_background = TRANSPARENT } },
	{
		"ellisonleao/gruvbox.nvim",
		opts = {
			contrast = "hard",
			italic = {
				strings = false,
				comments = false,
				operators = false,
				folds = false,
			},
			transparent_mode = TRANSPARENT,
		},
	},
	{ "catppuccin/nvim", name = "catppuccin", opts = {} },
}
