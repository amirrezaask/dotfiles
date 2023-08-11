return {
	{ "navarasu/onedark.nvim", opts = { style = "darker", transparent = true } },
	{ "rose-pine/neovim", name = "rose-pine", opts = { disable_italics = true, disable_background = true } },
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
			transparent_mode = true,
		},
	},
	{ "catppuccin/nvim", name = "catppuccin", opts = {} },
}
