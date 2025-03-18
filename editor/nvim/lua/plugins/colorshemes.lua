local transparent = os.getenv("NVIM_TRANSPARENT") or false
return {
	{
		"folke/tokyonight.nvim",

		opts = {
			style = "moon",
			transparent = transparent,
		},
	},
	{
		"rose-pine/neovim",
		name = "rose-pine",
		opts = {
			styles = {
				italic = false,
				transparency = transparent,
			},
		},
	},
	{
		"ellisonleao/gruvbox.nvim",
		opts = {
			contrast = "hard",
			transparent_mode = transparent,
		},
	},

	{ "catppuccin/nvim", name = "catppuccin", opts = { transparent_background = transparent } },
}
