return {
	{
		"folke/tokyonight.nvim",
		priority = 1000,
		config = function()
			require("tokyonight").setup({ transparent = true })
			vim.cmd.colorscheme("tokyonight-night")
		end,
	},
	{ "rose-pine/neovim", name = "rose-pine" },
	{ "catppuccin/nvim", name = "catppuccin" },
	{ "vague-theme/vague.nvim" },
}