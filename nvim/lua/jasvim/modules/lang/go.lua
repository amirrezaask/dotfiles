require("nvim-treesitter.install").ensure_installed "go"

require("lspconfig").gopls.setup {
  on_attach = lsp.on_attach,
}

onsave("*.go", function()
  vim.lsp.buf.format()
end)