return {
	"neovim/nvim-lspconfig",
	dependencies = { "williamboman/mason.nvim" },
	config = function()
		local function get_path_sep()
			if vim.fn.has("win32") == 1 then
				return "\\"
			else
				return "/"
			end
		end

		local sep = get_path_sep()

		vim.env.PATH = string.format("%s%smason%sbin:", (vim.fn.stdpath("data")), sep, sep) ..
			vim.env.PATH
		require("mason").setup({})
		local lsp_servers = {
			ocamllsp = {
				cmd = { string.format("%s/.opam/default/bin/%s", os.getenv("HOME"), "ocamllsp") },
				get_language_id = function(_, ftype)
					return ftype
				end,
			},
			gopls = {},
			lua_ls = {
				settings = {
					Lua = {
						telemetry = { enable = false },
						diagnostics = {
							globals = { "vim" },
						},
						workspace = {
							checkThirdParty = false,
							library = vim.api.nvim_get_runtime_file("", true),
						},
					},
				},
			},
			rust_analyzer = {},
			zls = {},
		}
		for server, config in pairs(lsp_servers) do
			require("lspconfig")[server].setup(config)
		end
		vim.diagnostic.config({ virtual_text = true })

		vim.api.nvim_create_autocmd("LspAttach", {
			callback = function(args)
				local bufnr = args.buf
				vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc",
					{ buf = bufnr })
				local buffer = function(desc)
					return { buffer = bufnr, desc = desc }
				end
				vim.keymap.set("n", "gd", vim.lsp.buf.definition, buffer("Goto Definition"))
				vim.keymap.set("n", "gD", vim.lsp.buf.declaration, buffer("Goto Declaration"))
				vim.keymap.set("n", "gi", vim.lsp.buf.implementation,
					buffer("Goto Implementation"))
				vim.keymap.set("n", "gr", vim.lsp.buf.references, buffer("Goto References"))
				vim.keymap.set("n", "R", vim.lsp.buf.rename, buffer("Rename"))
				vim.keymap.set("n", "K", vim.lsp.buf.hover, buffer("Hover"))
				vim.keymap.set("n", "<leader>f", vim.lsp.buf.format, buffer("Format"))
				vim.keymap.set("n", "gl", vim.diagnostic.open_float, buffer(""))
				vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, buffer("Next Diagnostic"))
				vim.keymap.set("n", "]d", vim.diagnostic.goto_next, buffer("Previous Diagnostic"))
				vim.keymap.set("n", "C", vim.lsp.buf.code_action, buffer("Code Actions"))
				vim.keymap.set("n", "<C-s>", vim.lsp.buf.signature_help, buffer("Signature Help"))
				vim.keymap.set("i", "<C-s>", vim.lsp.buf.signature_help, buffer("Signature Help"))
			end,
		})
		-- Hover and signature help windows have rounded borders
		vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover,
			{ border = "rounded" })
		vim.lsp.handlers["textDocument/signatureHelp"] =
			vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" })

		-- LspInfo window have rounded border
		require("lspconfig.ui.windows").default_options.border = "rounded"
	end,
}
