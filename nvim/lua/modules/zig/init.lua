
require"core.treesitter".install("zig")
require("lspconfig").zls.setup {
   on_attach = require"core.lsp".lsp_on_attach,
}
