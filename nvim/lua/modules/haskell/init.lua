require"core.treesitter".install("haskell")
require("lspconfig").hls.setup {
   on_attach = require"core.lsp".lsp_on_attach,
}
