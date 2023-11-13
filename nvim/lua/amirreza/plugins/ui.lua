return {
	{ "rose-pine/neovim",         name = "rose-pine",          opts = { disable_italics = true } },
	{ "ellisonleao/gruvbox.nvim", opts = { contrast = 'hard' } },
	{ 'navarasu/onedark.nvim',    opts = { style = 'darker' } },
	{ "catppuccin/nvim",          name = "catppuccin",         opts = { flavor = "macchiato" } },
	{ "EdenEast/nightfox.nvim" },
	{
		"folke/edgy.nvim",
		event = "VeryLazy",
		opts = {}
	},
	{
		"folke/noice.nvim",
		event = "VeryLazy",
		opts = {
			notify = { enabled = false }
		},
		dependencies = {
			"MunifTanjim/nui.nvim",
			"rcarriga/nvim-notify",
		}
	}
}
