require "treesitter".install("go")

require("lspconfig").gopls.setup {
   on_attach = require"lsp".lsp_on_attach,
}

require"onsave"("*.go", function()
    vim.lsp.buf.format()
end)
