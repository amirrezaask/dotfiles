require("nvim-treesitter.install").ensure_installed "yaml"
require("lspconfig").yamlls.setup {
  on_attach = lsp.on_attach,
}
