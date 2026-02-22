return {
	{
		"folke/tokyonight.nvim",
		priority = 1000,
		config = function()
			require("tokyonight").setup({ transparent = true })
		end,
	},
	{ "rose-pine/neovim", name = "rose-pine" },
	{ "catppuccin/nvim", name = "catppuccin" },
	{ "vague-theme/vague.nvim" },
	{
		"eldritch-theme/eldritch.nvim",
		lazy = true,
		name = "eldritch",
		opts = {
			transparent = true,
			styles = {
				sidebars = "transparent",
				floats = "transparent",
			},
		},
	},
}
