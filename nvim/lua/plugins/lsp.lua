return {
    -- Autocompletion popup
    {
        "hrsh7th/nvim-cmp",
        config = function()
            -- Autocompletion menu using nvim-cmp
            local capabilities = vim.lsp.protocol.make_client_capabilities()
            capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)
            local cmp = require "cmp"
            cmp.setup {
                window = {
                    completion = cmp.config.window.bordered(),
                    documentation = cmp.config.window.bordered(),
                },
                snippet = {
                    expand = function(args) vim.fn["vsnip#anonymous"](args.body) end,
                },
                mapping = cmp.mapping.preset.insert {
                    ["<CR>"] = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
                },
                sources = {
                    { name = "nvim_lsp" },
                    { name = "buffer" },
                    { name = "path" },
                },
            }
        end,
        dependencies = {
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/cmp-vsnip",
            "hrsh7th/vim-vsnip",
            "hrsh7th/cmp-path",
            "hrsh7th/cmp-buffer",
        },
    },
    { -- LSP + Mason + Null-ls: Neovim builtin LSP client + Mason for installing langauge servers + Null-Ls to integrate non LSP tools into LSP client
        "neovim/nvim-lspconfig",
        dependencies = {
            "williamboman/mason.nvim",
            "jose-elias-alvarez/null-ls.nvim",
        },
        config = function()
            -- Install all binaries
            local INSTALL_THESE = {
                -- LSP
                "gopls",
                "rust-analyzer",
                "lua-language-server",
                "zls",

                -- Formatters
                "stylua",
                "yamlfmt",
                "goimports",

                -- Linters
                "golangci-lint",
            }
            for _, pkg in ipairs(INSTALL_THESE) do -- ensure these tools are installed
                if not require("mason-registry").is_installed(pkg) then require("mason.api.command").MasonInstall { pkg } end
            end
            -- TODO(amirreza): find a better more cross platform way of joining paths.
            vim.env.PATH = string.format("%s/mason/bin", vim.fn.stdpath "data") .. vim.env.PATH
            require("mason").setup {}
            require("null-ls").setup {
                sources = {
                    require("null-ls").builtins.code_actions.gitsigns,
                    require("null-ls").builtins.diagnostics.golangci_lint,
                    require("null-ls").builtins.diagnostics.trail_space.with { disabled_filetypes = { "NvimTree" } },
                    require("null-ls").builtins.formatting.stylua,
                    require("null-ls").builtins.formatting.goimports,
                },
            }

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
            vim.api.nvim_create_autocmd("LspAttach", {
                callback = function(args)
                    local bufnr = args.buf
                    vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
                    local buffer = function(desc) return { buffer = bufnr, desc = desc } end
                    vim.keymap.set("n", "gd", vim.lsp.buf.definition, buffer "Goto Definition")
                    vim.keymap.set("n", "gD", vim.lsp.buf.declaration, buffer "Goto Declaration")
                    vim.keymap.set("n", "gi", vim.lsp.buf.implementation, buffer "Goto Implementation")
                    vim.keymap.set("n", "gr", vim.lsp.buf.references, buffer "Goto References")
                    vim.keymap.set("n", "R", vim.lsp.buf.rename, buffer "Rename")
                    vim.keymap.set("n", "K", vim.lsp.buf.hover, buffer "Hover")
                    vim.keymap.set("n", "gl", vim.diagnostic.open_float, buffer "")
                    vim.keymap.set("n", "[[", vim.diagnostic.goto_prev, buffer "Next Diagnostic")
                    vim.keymap.set("n", "]]", vim.diagnostic.goto_next, buffer "Previous Diagnostic")
                    vim.keymap.set("n", "C", vim.lsp.buf.code_action, buffer "Code Actions")
                    vim.keymap.set("n", "<C-s>", vim.lsp.buf.signature_help, buffer "Signature Help")
                    vim.keymap.set("i", "<C-s>", vim.lsp.buf.signature_help, buffer "Signature Help")
                end,
            })
            -- Hover and signature help windows have rounded borders
            vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" })
            vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help,
                { border = "rounded" })

            -- LspInfo window have rounded border
            require("lspconfig.ui.windows").default_options.border = "rounded"

            vim.api.nvim_create_autocmd("BufWritePre", {
                pattern = { "*.rs", "*.lua" },
                callback = function(_) vim.lsp.buf.format() end,
            })
        end,
    },
}
