return {
	{
		"ibhagwan/fzf-lua",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		enabled = vim.fn.has("win32") == 0,
		config = function()
			local fzfLua = require("fzf-lua")
			fzfLua.setup({
				keymap = {
					fzf = {
						["ctrl-q"] = "select-all+accept",
					},
				},
				defaults = {
					previewer = false,
				},
				commands = {
					actions = {
						["enter"] = function(selected)
							vim.cmd(selected[1])
						end,
					},
				},
			})
			vim.keymap.set("n", "<leader><leader>", fzfLua.files)
			vim.keymap.set("n", "<leader>b", fzfLua.buffers)
			vim.keymap.set("n", "<leader>h", fzfLua.help_tags)
			vim.keymap.set("n", "<C-p>", fzfLua.git_files)
			vim.keymap.set("n", "??", fzfLua.live_grep)
			vim.keymap.set("n", "<leader>o", fzfLua.lsp_document_symbols)
			vim.keymap.set("n", "<leader>O", fzfLua.lsp_live_workspace_symbols)
			vim.keymap.set("n", "<leader>;", fzfLua.commands)
		end,
	},
	{
		"nvim-telescope/telescope.nvim",
		enabled = vim.fn.has("win32") == 1, -- fzf-lua is far more performant and superior but on windows telescope is better.
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope-ui-select.nvim",
		},

		config = function()
			require("telescope").setup({
				defaults = {
					file_ignore_patterns = {
						"node_modules",
						"vendor",
					},
				},
			})
			require("telescope").load_extension("ui-select")

			vim.keymap.set("n", "<leader>ff", function()
				require("telescope.builtin").find_files({ previewer = false })
			end)
			vim.keymap.set("n", "<leader><leader>", function()
				require("telescope.builtin").find_files({ previewer = false })
			end)

			vim.keymap.set("n", "<leader>i", function()
				require("telescope.builtin").find_files({
					prompt_title = "Neovim Config",
					cwd = vim.fn.stdpath("config"),
					previewer = false,
				})
			end)

			vim.keymap.set("n", "<c-p>", function()
				require("telescope.builtin").git_files({ previewer = false })
			end)

			vim.keymap.set("n", "??", function()
				require("telescope.builtin").live_grep({ previewer = false })
			end)

			vim.keymap.set("n", "<leader>o", function()
				require("telescope.builtin").lsp_document_symbols({
					previewer = false,
				})
			end)

			vim.keymap.set("n", "<leader>O", function()
				require("telescope.builtin").lsp_dynamic_workspace_symbols({
					previewer = false,
				})
			end)

			vim.keymap.set("n", "<leader>h", function()
				require("telescope.builtin").help_tags({})
			end)
			vim.keymap.set("n", "<leader>b", function()
				require("telescope.builtin").buffers({})
			end)
		end,
	},
}
