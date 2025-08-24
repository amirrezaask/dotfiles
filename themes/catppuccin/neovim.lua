return {
	"catppuccin/nvim",
	name = "catppuccin",
	config = function()
		require("catppuccin").setup({ transparent = true })
		vim.cmd.colorscheme("catppuccin-mocha")
	end,
}
