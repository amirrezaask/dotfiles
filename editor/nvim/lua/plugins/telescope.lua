return {
	"nvim-telescope/telescope.nvim",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-telescope/telescope-ui-select.nvim",
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
		-- local theme = require("telescope.themes").get_dropdown
		local theme = function(opts)
			return opts
		end

		local telescope_keys = {
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
}
