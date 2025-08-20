return {
	lazy_spec = { 
		{
			"folke/tokyonight.nvim",
			config = function(_, opts)
				require("tokyonight").setup ({ transparent = true })
				vim.cmd.colorscheme("tokyonight-night")
			end
		}
	}
}
