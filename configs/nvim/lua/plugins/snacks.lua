return {
	"folke/snacks.nvim",
	config = function()
		require("snacks").setup {
			terminal = { enabled = true },
			indent = { enabled = true },
			picker = { enabled = true },
			dashboard = { enabled = true },
			input = { enabled = true },
			lazygit = { enabled = true },
		}


		local Snacks = require "snacks"
		local picker = Snacks.picker
		vim.keymap.set("n", "<leader><leader>", picker.files)
		vim.keymap.set("n", "<leader>j", picker.grep)
		vim.keymap.set({ "n", "v" }, "<leader>k", picker.grep_word)
		vim.keymap.set("n", "<leader>o", picker.lsp_symbols)
		vim.keymap.set("n", "<leader>O", picker.lsp_workspace_symbols)
		vim.keymap.set("n", "<leader>g", Snacks.lazygit.open)
		vim.keymap.set({ "n", "t" }, "<C-j>", Snacks.terminal.toggle)

		vim.lsp.buf.references = picker.lsp_references
		vim.lsp.buf.definition = picker.lsp_definitions
		vim.lsp.buf.implementation = picker.lsp_implementations
		vim.lsp.buf.document_symbol = picker.lsp_symbols
		vim.lsp.buf.workspace_symbol = picker.lsp_workspace_symbols
	end,
}
