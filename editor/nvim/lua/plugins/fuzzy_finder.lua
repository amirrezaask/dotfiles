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
			_G.IDE.Files = require("telescope.builtin").find_files
			_G.IDE.Buffers = require("telescope.builtin").buffers
			_G.IDE.Help = require("telescope.builtin").help_tags
			_G.IDE.GitFiles = require("telescope.builtin").git_files
			_G.IDE.GitCommits = require("telescope.builtin").git_commits
			_G.IDE.Grep = require("telescope.builtin").live_grep
			_G.IDE.DocumentSymbols = require("telescope.builtin").lsp_document_symbols
			_G.IDE.WorkspaceSymbols = require("telescope.builtin").lsp_dynamic_workspace_symbols
			_G.IDE.Commands = require("telescope.builtin").commands
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

			_G.IDE.Files = fzfLua.files
			_G.IDE.Buffers = fzfLua.buffers
			_G.IDE.Help = fzfLua.help_tags
			_G.IDE.GitFiles = fzfLua.git_files
			_G.IDE.GitCommits = fzfLua.git_commits
			_G.IDE.GitBranches = fzfLua.git_branches
			_G.IDE.Grep = fzfLua.live_grep
			_G.IDE.DocumentSymbols = fzfLua.lsp_document_symbols
			_G.IDE.WorkspaceSymbols = fzfLua.lsp_live_workspace_symbols
			_G.IDE.Commands = function()
				local commands = vim.fn.getcompletion("", "command")
				fzfLua.fzf_exec(commands, {
					prompt = "Command Palette > ",
					actions = {
						["default"] = function(selected)
							vim.cmd(selected[1])
						end,
					},
				})
			end
		end,
	},
}
