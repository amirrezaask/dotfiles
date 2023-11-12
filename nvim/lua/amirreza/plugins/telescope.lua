return {

	"nvim-telescope/telescope.nvim",
	dependencies = {
		"nvim-lua/plenary.nvim",
		{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
		"nvim-telescope/telescope-ui-select.nvim",
	},
	config = function()
		require("telescope").setup({
			defaults = {
				sorting_strategy = "ascending",
				layout_strategy = "horizontal",
				layout_config = {
					horizontal = {
						preview_cutoff = 180,
						prompt_position = "top",
						height = 0.6,
						width = 0.7,
						preview_width = 0.7,
					},
				},
			},
		})

		require("telescope").load_extension("fzf")  -- load fzf awesomnes into Telescope
		require("telescope").load_extension("ui-select") -- Use telescope for vim.ui.select
		local telescope_builtin = require("telescope.builtin")
		local no_preview = { previewer = false }

		vim.keymap.set("n", "<C-p>", function()
			telescope_builtin.git_files(no_preview)
		end, { desc = "Git Files" })
		vim.keymap.set("n", "<leader>b", function()
			telescope_builtin.buffers(no_preview)
		end, { desc = "Telescope Buffers" })
		vim.keymap.set("n", "<leader><leader>", function()
			telescope_builtin.find_files(no_preview)
		end, { desc = "Find Files" })
		vim.keymap.set("n", "<leader>ff", function()
			telescope_builtin.find_files(no_preview)
		end, { desc = "Find Files" })
		vim.keymap.set("n", "<leader>w", function()
			telescope_builtin.grep_string({ layout_config = { height = 0.7, width = 0.9 } })
		end, { desc = "Grep for word at point" })
		vim.keymap.set("n", "<leader>o", function()
			telescope_builtin.treesitter(no_preview)
		end, { desc = "Search Symbols In Current File" })
		vim.keymap.set("n", "??", function()
			telescope_builtin.live_grep({ layout_config = { height = 0.9, width = 0.9 } })
		end, { desc = "Live Grep" })
	end,
}