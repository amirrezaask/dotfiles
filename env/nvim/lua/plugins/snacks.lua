return {
	{ -- Collection of Awesome plugins by @folke
		"folke/snacks.nvim",
		dependencies = {
			"nvim-tree/nvim-web-devicons",
		},
		opts = {
			dashboard = { enabled = true },
			picker = { enabled = true },
			indent = { enabled = true },
			input = { enabled = true },
			notifier = { enabled = true },
			terminal = { enabled = true },
		},
		config = function(_, opts)
			require("snacks").setup(opts)
			local Snacks = require("snacks")
			vim.keymap.set("n", "<leader><leader>", Snacks.picker.files)
			vim.keymap.set("n", "<leader>j", Snacks.picker.grep)
			vim.keymap.set({ "n", "v" }, "<leader>k", Snacks.picker.grep_word)
			vim.keymap.set("n", "<leader>O", Snacks.picker.lsp_workspace_symbols)
			vim.keymap.set({ "n", "t" }, "<C-S-j>", Snacks.terminal.toggle)

			vim.lsp.buf.references = Snacks.picker.lsp_references
			vim.lsp.buf.definition = Snacks.picker.lsp_definitions
			vim.lsp.buf.implementation = Snacks.picker.lsp_implementations
			vim.lsp.buf.document_symbol = Snacks.picker.lsp_symbols
			vim.lsp.buf.workspace_symbol = Snacks.picker.lsp_workspace_symbols

			vim.cmd([[ hi! link WarningMsg SnacksPickerPathIgnored ]])
		end,
	},
}
