require("nvim-treesitter.install").ensure_installed "zig"
require("lspconfig").zls.setup {
  on_attach = lsp.on_attach,
}
