local lsp = require"core.lsp"
local rt = require"rust-tools"

rt.setup({
    server = {
        on_attach = function(_, bufnr)
            lsp.on_attach(_, bufnr)
            vim.keymap.set("n", "C", rt.hover_actions.hover_actions, { buffer = bufnr })
            vim.keymap.set("n", "ga", rt.code_action_group.code_action_group, { buffer = bufnr })
        end,
    },
})

