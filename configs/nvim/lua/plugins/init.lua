return {
	{ "stevearc/oil.nvim", opts = {} },

	"tpope/vim-surround",
	"tpope/vim-unimpaired",
	{
		"folke/tokyonight.nvim",
		config = function()
			vim.cmd("colorscheme tokyonight-night")
		end,
	},
	{
		"catppuccin/nvim",
		name = "catppuccin",
		config = function()
			vim.cmd("colorscheme catppuccin-macchiato")
		end,
	},
}
