-- Lazy: Installing Plugin manager
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
    if vim.v.shell_error ~= 0 then
        vim.api.nvim_echo({
            { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
            { out,                            "WarningMsg" },
            { "\nPress any key to exit..." },
        }, true, {})
        vim.fn.getchar()
        os.exit(1)
    end
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    {
        "folke/tokyonight.nvim",
        opts = {
            style = 'moon',
            transparent = true,
        }
    },
    {
        "rose-pine/neovim",
        name = 'rose-pine',
        opts = {
            styles = {
                italic = false,
                transparency = true,
            }
        }
    },

    { "catppuccin/nvim", name = "catppuccin", opts = { transparent_background = true } },

    { -- AI Assistant
        "supermaven-inc/supermaven-nvim",
        config = function()
            require("supermaven-nvim").setup({})
        end,
    },
    { -- Git Client
        "tpope/vim-fugitive",
    },

    { -- File manager
        'stevearc/oil.nvim',
        opts = {
            buf_options = {
                buflisted = true,
                bufhidden = "hide",
            },
        }
    },

    "nvim-pack/nvim-spectre", -- Search and replace in all project files

    {
        "nvim-treesitter/nvim-treesitter",
        dependencies = {
            { "folke/ts-comments.nvim", opts = {} },
        },
        config = function()
            vim.o.foldmethod = 'expr'                     -- Use expression for folding
            vim.o.foldexpr = 'nvim_treesitter#foldexpr()' -- Set Tree-sitter folding expression
            vim.o.foldenable = false                      -- Start with all folds open
            require("nvim-treesitter.configs").setup({
                ensure_installed = { "lua", "go", "gomod", "markdown", "php", "c", "cpp" },
                highlight = { enable = true },
            })
        end
    },

    {

        "nvim-telescope/telescope.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "nvim-telescope/telescope-ui-select.nvim",
        },

        config = function()
            require "telescope".setup({
                defaults = {
                    file_ignore_patterns = {
                        "node_modules",
                        "vendor"
                    }
                }
            })
            require("telescope").load_extension("ui-select")
            -- local theme = require("telescope.themes").get_dropdown
            local theme = function(opts) return opts end

            local telescope_keys = {
                ["<leader>p"] = { "git_files", previewer = false, theme = theme },
                ["<c-p>"] = { "git_files", previewer = false, theme = theme },
                ["<leader><leader>"] = { "find_files", previewer = false, theme = theme },
                ["??"] = "live_grep",
                ["<leader>h"] = { "help_tags", previewer = false, theme = theme },
                ["<leader>b"] = { "buffers", previewer = false, theme = theme },
            }


            for k, v in pairs(telescope_keys) do
                if type(v) == "string" then
                    vim.keymap.set("n", k, function()
                        require "telescope.builtin"[v]({})
                    end, {})
                elseif type(v) == "function" then
                    vim.keymap.set("n", k, v)
                elseif type(v) == "table" then
                    vim.keymap.set("n", k, function()
                        local current_theme = v['theme'] or function(opts) return opts end
                        require "telescope.builtin"[v[1]](current_theme({
                            previewer = v['previewer']
                        }))
                    end)
                end
            end
        end
    },


    { -- Autoformat
        "stevearc/conform.nvim",
        opts = {

            format_on_save = {
                -- These options will be passed to conform.format()
                timeout_ms = 500,
                lsp_format = "fallback",
            },
            formatters_by_ft = {
                lua = { "stylua", lsp_format = "fallback" },
                go = { "goimports", "gofmt" },
            },
        }
    },

    { -- LSP
        "neovim/nvim-lspconfig",
        dependencies = {
            "williamboman/mason.nvim",
            "williamboman/mason-lspconfig.nvim",
        },
        config = function()
            require("lspconfig.ui.windows").default_options.border = "single"

            vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover,
                { border = "rounded" })

            vim.lsp.handlers["textDocument/signatureHelp"] =
                vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" })

            require("mason").setup()
            require("mason-lspconfig").setup({ ensure_installed = { "gopls" } })
            local lsp_servers = {
                gopls = {},
                intelephense = {},
                lua_ls = {
                    settings = {
                        Lua = {
                            telemetry = { enable = false },
                            diagnostics = {
                                globals = { "vim" },
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
                    vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc",
                        { buf = bufnr })

                    local map = function(mode, key, fn, desc)
                        vim.keymap.set(mode, key, fn, { buffer = bufnr, desc = "LSP: " .. desc })
                    end
                    local references = vim.lsp.buf.references
                    local implementation = vim.lsp.buf.implementation
                    local has_tele, tele = pcall(require, "telescope.builtin")
                    if has_tele then
                        references = tele.lsp_references
                        implementation = tele.lsp_implementations
                    end
                    map("n", "[[", vim.diagnostic.goto_prev, "Diagnostics: Next")
                    map("n", "]]", vim.diagnostic.goto_next, "Diagnostics: Previous")
                    map("n", "C-]", vim.lsp.buf.definition, "[g]oto definition")
                    map("n", "gd", vim.lsp.buf.definition, "[g]oto [d]efinition")
                    map("n", "gD", vim.lsp.buf.declaration, "[g]oto [D]eclaration")
                    map("n", "gI", implementation, "[g]oto [i]mplementation")
                    map("n", "gr", references, "[g]oto [r]eferences")
                    map("n", "R", vim.lsp.buf.rename, "Rename")
                    map("n", "K", vim.lsp.buf.hover, "Hover")
                    map("n", "C", vim.lsp.buf.code_action, "Code Actions")
                    map("n", "<leader>f", vim.lsp.buf.format, "Format")
                    map({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, "Signature Help")
                    vim.diagnostic.config({ virtual_text = false })
                end,
            })
        end

    },
    { -- Autocomplete menu
        'hrsh7th/nvim-cmp',
        dependencies = {
            'hrsh7th/cmp-nvim-lsp',
            'hrsh7th/cmp-buffer',

        },
        config = function()
            local cmp_select = { behavior = require("cmp").SelectBehavior.Select }
            local cmp = require("cmp")
            cmp.setup({
                preselect = require("cmp.types").cmp.PreselectMode.None,
                snippet = {
                    expand = function(args)
                        vim.snippet.expand(args.body)
                    end,
                },
                mapping = cmp.mapping.preset.insert({
                    ["<C-p>"] = cmp.mapping.select_prev_item(cmp_select),
                    ["<C-n>"] = cmp.mapping.select_next_item(cmp_select),
                    ["<C-y>"] = cmp.mapping.confirm({ select = true }),
                    ["<CR>"] = cmp.mapping.confirm({ select = false }),
                    ["<C-Space>"] = cmp.mapping.complete(),
                }),
                sources = {
                    { name = "nvim_lsp" },
                    { name = "buffer" }
                },
            })
        end
    },
})
