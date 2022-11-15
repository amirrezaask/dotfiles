require("treesitter").install "c"
require("treesitter").install "cpp"

require("lspconfig").clangd.setup {
  on_attach = require("lsp").lsp_on_attach,
}
