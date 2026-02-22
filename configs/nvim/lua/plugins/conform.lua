return {
	"stevearc/conform.nvim",
	opts = {
		formatters_by_ft = {
			php = nil,
			go = { "goimports" },
			lua = { "stylua" },
			json = { "jq" },
			javascript = { "eslint_d" },
			typescript = { "eslint_d" },
			javascriptreact = { "eslint_d" },
			typescriptreact = { "eslint_d" },
		},
		format_on_save = function(bufnr)
			local ft = vim.bo[bufnr].filetype
			if
				({ php = true, javascript = true, typescript = true, javascriptreact = true, typescriptreact = true })[ft]
			then
				return { timeout_ms = 500, lsp_fallback = false }
			end
			return { timeout_ms = 500, lsp_fallback = true }
		end,
	},
}
