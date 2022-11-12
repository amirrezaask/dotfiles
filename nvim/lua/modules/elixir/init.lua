require"core.treesitter".install("elixir")
require"lspconfig".elixirls.setup {
   on_attach = require"core.lsp".lsp_on_attach,
   cmd = { os.getenv("HOME") .. "/.local/elixir-ls/language_server.sh" }
}
