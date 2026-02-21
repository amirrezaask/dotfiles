return {
	"folke/snacks.nvim",
	priority = 1000,
	lazy = false,
	dependencies = { "nvim-tree/nvim-web-devicons" },
	---@type snacks.Config
	opts = {
		bigfile = { enabled = true },
		indent = { enabled = true },
		picker = { enabled = true },
		notifier = { enabled = true },
		quickfile = { enabled = true },
		statuscolumn = { enabled = true },
		dashboard = { enabled = true },
	},
	keys = {
		{
			"<leader><leader>",
			function()
				Snacks.picker.files()
			end,
			mode = "n",
			silent = true,
		},
		{
			"<leader>pf",
			function()
				Snacks.picker.git_files()
			end,
			mode = "n",
			silent = true,
		},
		{
			"gd",
			function()
				Snacks.picker.lsp_definitions()
			end,
			mode = "n",
			silent = true,
		},
		{
			"grr",
			function()
				Snacks.picker.lsp_references()
			end,
			mode = "n",
			silent = true,
		},
		{
			"gri",
			function()
				Snacks.picker.lsp_implementations()
			end,
			mode = "n",
			silent = true,
		},
		{
			"<leader>j",
			function()
				Snacks.picker.grep()
			end,
			mode = { "n", "v" },
			silent = true,
		},
		{
			"<leader>J",
			function()
				Snacks.picker.grep_word()
			end,
			mode = { "n", "v" },
			silent = true,
		},
	},
}

