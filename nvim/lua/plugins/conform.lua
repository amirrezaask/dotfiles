require("conform").setup {
    format_on_save = function(bufnr)
        if vim.bo[bufnr].filetype == "php" then
            return {}
        else
            return { timeout_ms = 500, lsp_format = "fallback" }
        end
    end,
    lua = { "stylua" },
    go = { 'goimports' },
}
