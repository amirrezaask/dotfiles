return {
	"ibhagwan/fzf-lua",
	enabled = false,
	dependencies = { "nvim-tree/nvim-web-devicons" },
	config = function()
		require("fzf-lua").setup {
			"telescope",
			keymap = {
				fzf = {
					["ctrl-q"] = "select-all+accept",
				},
			},
		}
		local FzfLua = require "fzf-lua"
		vim.keymap.set("n", "<leader><leader>", FzfLua.files)
		vim.keymap.set("n", "<leader>j", FzfLua.live_grep)
		vim.keymap.set({ "n" }, "<leader>k", FzfLua.grep_cword)
		vim.keymap.set({ "v" }, "<leader>k", FzfLua.grep_visual)
		vim.keymap.set("n", "<leader>o", FzfLua.lsp_document_symbols)
		vim.keymap.set("n", "<leader>O", FzfLua.lsp_workspace_symbols)

		vim.lsp.buf.references = FzfLua.lsp_references
		vim.lsp.buf.definition = FzfLua.lsp_definitions
		vim.lsp.buf.implementation = FzfLua.lsp_implementations
		vim.lsp.buf.document_symbol = FzfLua.lsp_symbols
		vim.lsp.buf.workspace_symbol = FzfLua.lsp_workspace_symbols
	end,
}
