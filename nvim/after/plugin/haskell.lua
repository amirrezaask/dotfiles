require("treesitter").install "haskell"
require("lspconfig").hls.setup {
  on_attach = require("lsp").lsp_on_attach,
}
