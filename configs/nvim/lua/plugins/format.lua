return {  -- Code Autoformat
	"stevearc/conform.nvim",
	opts = {
		formatters_by_ft = {
			php = nil,
			go = { "goimports" },
			lua = { "stylua" },
			json = { "jq" },
		},
		format_on_save = function(bufnr)
			-- Skip formatting for PHP files
			if vim.bo[bufnr].filetype == "php" then
				return false
			end
			return { timeout_ms = 500, lsp_fallback = true }
		end,
	},
}
