_G.FuzzyFinder = {}

return {
	{
		"nvim-telescope/telescope.nvim",
		enabled = vim.fn.has("win32") == 1,
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope-ui-select.nvim",
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make", enabled = vim.fn.has("unix") == 1 },
		},

		config = function()
			require("telescope").setup({
				defaults = {
					file_ignore_patterns = {
						"node_modules",
						-- "vendor"
					},
				},
			})
			require("telescope").load_extension("ui-select")
			_G.FuzzyFinder.FindFiles = require("telescope.builtin").find_files
			_G.FuzzyFinder.Buffers = require("telescope.builtin").buffers
			_G.FuzzyFinder.HelpTags = require("telescope.builtin").help_tags
			_G.FuzzyFinder.GitFiles = require("telescope.builtin").git_files
			_G.FuzzyFinder.Grep = require("telescope.builtin").live_grep
		end,
	},
	{
		"ibhagwan/fzf-lua",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		enabled = vim.fn.has("unix") == 1,
		config = function()
			local fzfLua = require("fzf-lua")
			fzfLua.setup({
				defaults = {
					previewer = false,
				},
			})

			_G.FuzzyFinder.FindFiles = fzfLua.files
			_G.FuzzyFinder.Buffers = fzfLua.buffers
			_G.FuzzyFinder.HelpTags = fzfLua.help_tags
			_G.FuzzyFinder.GitFiles = fzfLua.git_files
			_G.FuzzyFinder.Grep = fzfLua.grep_project
		end,
	},
}
