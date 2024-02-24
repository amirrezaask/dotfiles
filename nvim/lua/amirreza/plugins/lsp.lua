return { -- LSP: Language Server Protocol
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            { -- Like the panel in vscode, shows you errors and warnings from LSP
                "folke/trouble.nvim",
                dependencies = { "nvim-tree/nvim-web-devicons" },
                config = function()
                    require "trouble".setup({})
                    vim.keymap.set("n", "<leader>j", ":TroubleToggle<CR>")
                end,
            },
            { -- Package manager for neovim install lsp servers in neovim path.
                "williamboman/mason.nvim",
                config = function()
                    local function get_path_sep()
                        if IS_WINDOWS then
                            return "\\"
                        else
                            return "/"
                        end
                    end

                    local sep = get_path_sep()

                    if IS_WINDOWS then
                        vim.env.PATH = string.format("%s%smason%sbin;", (vim.fn.stdpath("data")), sep, sep) ..
                            vim.env.PATH
                    else
                        vim.env.PATH = string.format("%s%smason%sbin:", (vim.fn.stdpath("data")), sep, sep) ..
                            vim.env.PATH
                    end
                    require("mason").setup({})
                end
            },
            { 'j-hui/fidget.nvim', opts = {} },
        },
        config = function()
            local lsp_servers = {
                gopls = {},
                lua_ls = {
                    settings = {
                        Lua = {
                            telemetry = { enable = false },
                            diagnostics = {
                                globals = { "vim" },
                            },
                            workspace = {
                                checkThirdParty = false,
                                library = vim.api.nvim_get_runtime_file("", true),
                            },
                        },
                    },
                },
                rust_analyzer = {},
                zls = {},
            }

            for server, config in pairs(lsp_servers) do
                require("lspconfig")[server].setup(config)
            end

            -- Hover and signature help windows have rounded borders
            vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" })
            vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help,
                { border = "rounded" })
            -- LspInfo window have rounded border
            require("lspconfig.ui.windows").default_options.border = "rounded"

            vim.api.nvim_create_autocmd("LspAttach", {
                callback = function(args)
                    local bufnr = args.buf
                    vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc",
                        { buf = bufnr })

                    local bind = function(mode, key, fn, desc)
                        vim.keymap.set(mode, key, fn, { buffer = bufnr, desc = desc })
                    end

                    bind("n", "gd", vim.lsp.buf.definition, "Goto Definition")
                    bind("n", "gD", vim.lsp.buf.declaration, "Goto Declaration")
                    bind("n", "gi", vim.lsp.buf.implementation, "Goto Implementation")
                    bind("n", "gr", vim.lsp.buf.references, "Goto References")
                    bind("n", "R", vim.lsp.buf.rename, "Rename")
                    bind("n", "K", vim.lsp.buf.hover, "Hover")
                    bind("n", "<leader>l", vim.diagnostic.open_float, "Floating Diagnostics")
                    bind("n", "<leader>q", vim.diagnostic.setloclist, "Set Loclist")
                    bind("n", "<leader>f", vim.lsp.buf.format, "Format")
                    bind("n", "gl", vim.diagnostic.open_float, "")
                    bind("n", "[d", vim.diagnostic.goto_prev, "Next Diagnostic")
                    bind("n", "]d", vim.diagnostic.goto_next, "Previous Diagnostic")
                    bind("n", "C", vim.lsp.buf.code_action, "Code Actions")
                    bind("n", "<C-s>", vim.lsp.buf.signature_help, "Signature Help")
                    bind("i", "<C-s>", vim.lsp.buf.signature_help, "Signature Help")

                    -- I hate it when I am writing a piece of code that things start to get all red.
                    vim.diagnostic.config({ virtual_text = false })
                end,
            })
            -- Autoformat for golang
            vim.api.nvim_create_autocmd("BufWritePre", {
                pattern = "*.go",
                callback = function()
                    vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } }, apply = true })
                    vim.lsp.buf.format()
                end,
            })
        end,
    },

}
