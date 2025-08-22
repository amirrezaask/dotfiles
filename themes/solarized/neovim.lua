return {
	-- add solarized
	"maxmx03/solarized.nvim",
	config = function()
		require("solarized").setup({
			palette = "solarized",
			variant = "autumn",
			styles = {
				constants = { bold = true },
			},
		})
		vim.cmd("colorscheme solarized")
	end,
}
