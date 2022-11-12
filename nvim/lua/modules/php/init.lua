require "core.treesitter".install("php")
require("lspconfig").intelephense.setup {
   on_attach = require"core.lsp".lsp_on_attach,
}
