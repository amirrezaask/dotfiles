require"core.treesitter".install("c")
require"core.treesitter".install("cpp")

require("lspconfig").clangd.setup {
   on_attach = require"core.lsp".lsp_on_attach,
}
