local lsp = require"lsp"
local rt = require"rust-tools"

require"treesitter".install("rust")

rt.setup({
    server = {
        on_attach = function(_, bufnr)
            lsp.lsp_on_attach(_, bufnr)
            vim.keymap.set("n", "C", rt.hover_actions.hover_actions, { buffer = bufnr })
            vim.keymap.set("n", "ga", rt.code_action_group.code_action_group, { buffer = bufnr })
        end,
    },
})
