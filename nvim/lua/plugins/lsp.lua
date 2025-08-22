return {
	"mason-org/mason-lspconfig.nvim",
	opts = {
		ensure_installed = { "lua_ls", "gopls" },
	},
	dependencies = {
		{ "mason-org/mason.nvim", opts = {} },
		{
			"neovim/nvim-lspconfig",
			config = function()
				-- Default Keybindings
				-- see :h lsp-defaults
				-- see :h vim.lsp.buf.tagfunc()
				vim.api.nvim_create_autocmd("LspAttach", {
					callback = function(args)
						-- local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
						-- if client:supports_method('textDocument/completion') then
						--   vim.lsp.completion.enable(true, client.id, args.buf, {autotrigger = true})
						-- end
						vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
						vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf })
					end,
				})
				vim.lsp.config(
					"lua_ls",
					{ settings = { Lua = { workspace = { library = vim.api.nvim_get_runtime_file("", true) } } } }
				)
			end,
		},
	},
}
