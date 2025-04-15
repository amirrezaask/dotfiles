vim.lsp.enable("lua_ls")
vim.lsp.enable("gopls")
vim.lsp.enable("intelephense")


local keymap = vim.keymap.set
vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
        local has_fzf, _ = pcall(require, "fzf-lua")
        vim.keymap.set("n", "[[", function()
            vim.diagnostic.jump({ count = -1 })
        end, { buffer = args.buf })
        keymap("n", "]]", function()
            vim.diagnostic.jump({ count = 1 })
        end, { buffer = args.buf })
        keymap("n", "C-]", has_fzf and require("fzf-lua").lsp_definition or vim.lsp.buf.definition,
            { buffer = args.buf })
        keymap("n", "gd", has_fzf and require("fzf-lua").lsp_definition or vim.lsp.buf.definition,
            { buffer = args.buf })
        keymap("n", "gD", has_fzf and require("fzf-lua").lsp_declaration or vim.lsp.buf.declaration,
            { buffer = args.buf })
        keymap("n", "gr", has_fzf and require("fzf-lua").lsp_references or vim.lsp.buf.references,
            { buffer = args.buf })
        keymap("n", "gi", has_fzf and require("fzf-lua").lsp_implementation or vim.lsp.buf.implementation,
            { buffer = args.buf })
        keymap("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
        keymap("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
        keymap("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
        keymap({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, { buffer = args.buf })
        keymap("n", "<leader>l", vim.diagnostic.open_float, { buffer = args.buf })
        keymap("n", "<leader>q", vim.diagnostic.setloclist, { buffer = args.buf })
        vim.api.nvim_create_autocmd("BufWritePre", {
            buffer = args.buf,
            callback = function(args)
                local old_print = print
                print = function(...) end
                vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } }, apply = true })
                print = old_print
                vim.lsp.buf.format({ bufnr = args.buf })
                vim.cmd.write()
            end,
        })
    end,
})
