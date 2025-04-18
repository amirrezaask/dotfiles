vim.lsp.enable({ "lua_ls", "gopls", "intelephense" })

local keymap = vim.keymap.set
LspDeclaration = vim.lsp.buf.declaration
LspDefinition = vim.lsp.buf.definition
LspReferences = vim.lsp.buf.references
LspImplementation = vim.lsp.buf.implementation

vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
        vim.keymap.set("n", "[[", function()
            vim.diagnostic.jump({ count = -1 })
        end, { buffer = args.buf })
        keymap("n", "]]", function()
            vim.diagnostic.jump({ count = 1 })
        end, { buffer = args.buf })
        keymap("n", "C-]", LspDefinition,
            { buffer = args.buf })
        keymap("n", "gd", LspDefinition,
            { buffer = args.buf })
        keymap("n", "gD", LspDeclaration,
            { buffer = args.buf })
        keymap("n", "gr", LspReferences,
            { buffer = args.buf })
        keymap("n", "gi", LspImplementation,
            { buffer = args.buf })
        keymap("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
        keymap("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
        keymap("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
        keymap({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, { buffer = args.buf })
        keymap("n", "<leader>l", vim.diagnostic.open_float, { buffer = args.buf })
        keymap("n", "<leader>q", vim.diagnostic.setloclist, { buffer = args.buf })
    end,
})
