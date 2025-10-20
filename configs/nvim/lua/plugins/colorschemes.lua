return {
	{
		"vague2k/vague.nvim",
		enabled = COLORSCHEME == "vague",
		config = function()
			require("vague").setup({
				background = "dark",
				transparent = true,
			})
			vim.cmd("colorscheme vague")
		end,
	},
	{
		"folke/tokyonight.nvim",
		enabled = COLORSCHEME == "tokyonight",
		config = function()
			require("tokyonight").setup({
				style = "night",
				transparent = true,
			})
			vim.cmd("colorscheme tokyonight-night")
			vim.cmd([[
				hi Normal guibg=NONE
				hi NormalFloat guibg=NONE
				hi SignColumn guibg=NONE
			]])
		end,
	},
	{
		"catppuccin/nvim",
		name = "catppuccin",
		enabled = COLORSCHEME == "catppuccin",
		config = function()
			vim.cmd("colorscheme catppuccin-macchiato")
		end,
	},
}
