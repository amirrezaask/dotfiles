require"treesitter".install("zig")
require("lspconfig").zls.setup {
   on_attach = require"lsp".lsp_on_attach,
}
