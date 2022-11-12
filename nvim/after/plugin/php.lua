require "treesitter".install("php")
require("lspconfig").intelephense.setup {
   on_attach = require"lsp".lsp_on_attach,
}
