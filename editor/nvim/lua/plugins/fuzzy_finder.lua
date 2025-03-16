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
			local theme = function(opts)
				return opts
			end
			require("telescope").load_extension("ui-select")
			local telescope_keys = {
				["<leader>i"] = function()
					require("telescope.builtin").find_files({
						prompt_title = "Neovim Config",
						cwd = vim.fn.stdpath("config"),
						previewer = false,
					})
				end,
				["<leader>p"] = { "git_files", previewer = false, theme = theme },
				["<c-p>"] = { "git_files", previewer = false, theme = theme },
				["<leader><leader>"] = { "find_files", previewer = false, theme = theme },
				["??"] = "live_grep",
				["<leader>h"] = { "help_tags", previewer = false, theme = theme },
				["<leader>b"] = { "buffers", previewer = false, theme = theme },
			}

			for k, v in pairs(telescope_keys) do
				if type(v) == "string" then
					vim.keymap.set("n", k, function()
						require("telescope.builtin")[v]({})
					end, {})
				elseif type(v) == "function" then
					vim.keymap.set("n", k, v)
				elseif type(v) == "table" then
					vim.keymap.set("n", k, function()
						local current_theme = v["theme"] or function(opts)
							return opts
						end
						require("telescope.builtin")[v[1]](current_theme({
							previewer = v["previewer"],
						}))
					end)
				end
			end
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
}
