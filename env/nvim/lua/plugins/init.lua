return {
	{ "stevearc/oil.nvim", opts = {} },
	"tpope/vim-surround",
	{
		"jake-stewart/multicursor.nvim",
		branch = "1.0",
		config = function()
			local mc = require("multicursor-nvim")
			mc.setup()

			local set = vim.keymap.set

			-- Add or skip cursor above/below the main cursor.
			set({ "n", "x" }, "<M-k>", function() mc.lineAddCursor(-1) end)
			set({ "n", "x" }, "<M-j>", function() mc.lineAddCursor(1) end)
			-- set({ "n", "x" }, "<leader><up>", function() mc.lineSkipCursor(-1) end)
			-- set({ "n", "x" }, "<leader><down>", function() mc.lineSkipCursor(1) end)

			-- Add or skip adding a new cursor by matching word/selection
			set({ "n", "x" }, "<M-d>", function() mc.matchAddCursor(1) end)
			-- set({ "n", "x" }, "<leader>s", function() mc.matchSkipCursor(1) end)
			set({ "n", "x" }, "<M-D>", function() mc.matchAddCursor(-1) end)
			-- set({ "n", "x" }, "<leader>S", function() mc.matchSkipCursor(-1) end)

			-- Add and remove cursors with control + left click.
			set("n", "<c-leftmouse>", mc.handleMouse)
			set("n", "<c-leftdrag>", mc.handleMouseDrag)
			set("n", "<c-leftrelease>", mc.handleMouseRelease)

			-- Disable and enable cursors.
			set({ "n", "x" }, "<c-q>", mc.toggleCursor)

			-- Mappings defined in a keymap layer only apply when there are
			-- multiple cursors. This lets you have overlapping mappings.
			mc.addKeymapLayer(function(layerSet)
				-- Select a different cursor as the main one.
				layerSet({ "n", "x" }, "<left>", mc.prevCursor)
				layerSet({ "n", "x" }, "<right>", mc.nextCursor)

				-- Delete the main cursor.
				layerSet({ "n", "x" }, "x", mc.deleteCursor)

				-- Enable and clear cursors using escape.
				layerSet("n", "<esc>", function()
					if not mc.cursorsEnabled() then
						mc.enableCursors()
					else
						mc.clearCursors()
					end
				end)
			end)
		end
	},
	"tpope/vim-unimpaired",
	{
		"nvim-tree/nvim-tree.lua",
		version = "*",
		lazy = false,
		dependencies = {
			"nvim-tree/nvim-web-devicons",
		},
		config = function()
			require("nvim-tree").setup({})
			vim.keymap.set("n", "<leader>e", ":NvimTreeToggle<CR>")
		end,
	},
}
