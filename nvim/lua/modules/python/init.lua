require("lspconfig").jedi_language_server.setup {
   on_attach = require"core.lsp".lsp_on_attach,
}
