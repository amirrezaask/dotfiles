require "core.treesitter".install("go")

require("lspconfig").gopls.setup {
   on_attach = require"core.lsp".lsp_on_attach,
}
