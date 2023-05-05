-- ==========================================================================
-- ============================ Options =====================================
-- ==========================================================================
vim.opt.number = true -- Line numbers
vim.opt.relativenumber = true -- Relative line numbers
vim.opt.errorbells = false
vim.opt.smartindent = true
vim.opt.wrap = false
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = true
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.termguicolors = true
vim.opt.signcolumn = "yes"
vim.opt.isfname:append "@-@"
vim.opt.updatetime = 50
vim.opt.guicursor = "" -- Don't style cursor in different modes, just a box would suffice
vim.opt.shortmess:append "c" -- Don't pass messages to |ins-completion-menu|.
vim.opt.shortmess:append "I" -- No Intro message
vim.opt.clipboard:append "unnamedplus" -- use system clipboard as default register.
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.cursorline = true
vim.opt.sw = 4
vim.opt.ts = 4
vim.opt.expandtab = true
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.opt.laststatus = 2
vim.opt.timeoutlen = 300

-- ==========================================================================
-- ========================= Plugins ========================================
-- ==========================================================================
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system {
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    }
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup {
    {
        "nvim-lualine/lualine.nvim", -- Statusline
        dependencies = { "nvim-tree/nvim-web-devicons", opt = true },
        config = function() require("lualine").setup {} end,
    },
    {
        "amirrezaask/themes",
        config = function() pcall(vim.cmd.colorscheme, "rose-pine") end,
        dependencies = {
            {
                "ellisonleao/gruvbox.nvim", -- Best theme of all time
                config = function()
                    require("gruvbox").setup {
                        contrast = "hard",
                        italic = {
                            strings = false,
                            comments = false,
                            operators = false,
                            folds = false,
                        },
                    }
                end,
            },
            {
                "bluz71/vim-nightfly-colors",
                config = function() vim.nightflyTransparent = true end,
            },
            "shaunsingh/nord.nvim",
            "shaunsingh/oxocarbon.nvim",
            {
                "rose-pine/neovim",
                name = "rose-pine",
                config = function() require("rose-pine").setup { disable_background = true } end,
            },
            {
                "folke/tokyonight.nvim",
                config = function() require("tokyonight").setup { transparent = true } end,
            }, -- folkkkkkeeeeee
        },
    },

    -- Comment code
    {
        "numToStr/Comment.nvim",
        config = function() require("Comment").setup() end,
    },
    { -- telescope: Fuzzy finding and searching interface
        "nvim-telescope/telescope.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
            { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
            "nvim-telescope/telescope-ui-select.nvim",
        },
        config = function()
            require("telescope").setup {
                extensions = {
                    ["ui-select"] = {
                        require("telescope.themes").get_dropdown {},
                    },
                },
            } -- Best fuzzy finder
            require("telescope").load_extension "fzf" -- load fzf awesomnes into Telescope
            require("telescope").load_extension "ui-select" -- Use telescope for vim.ui.select
        end,
    },
    { -- Treesitter syntax highlighting and text objects.
        "nvim-treesitter/nvim-treesitter",
        dependencies = { "nvim-treesitter/nvim-treesitter-textobjects", "nvim-treesitter/playground" },
        config = function()
            require("nvim-treesitter.configs").setup {
                -- Setup treesitter text objects module + highlight
                ensure_installed = { "json", "yaml", "c", "cpp", "lua", "rust", "go", "python", "php" },
                context_commentstring = { enable = true },
                highlight = { enable = true, additional_vim_regex_highlighting = false },
                textobjects = {
                    select = {
                        enable = true,
                        lookahead = true,
                        keymaps = {
                            ["af"] = "@function.outer",
                            ["if"] = "@function.inner",
                            ["ac"] = "@class.outer",
                            ["ic"] = "@class.inner",
                        },
                    },
                },
            }
            -- Install all treesitter parsers.
            pcall(require("nvim-treesitter.install").update { with_sync = true })
        end,
    },
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
            { "williamboman/mason.nvim", dependencies = { "williamboman/mason-lspconfig.nvim" } },
            "jose-elias-alvarez/null-ls.nvim",
        },
        config = function()
            require("mason").setup {}
            for _, pkg in ipairs { "stylua", "golangci-lint", "goimports", "yamlfmt" } do -- ensure these tools are installed
                if not require("mason-registry").is_installed(pkg) then require("mason.api.command").MasonInstall { pkg } end
            end

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
            local mason_lspconfig = require "mason-lspconfig"
            mason_lspconfig.setup {
                ensure_installed = vim.tbl_keys(lsp_servers),
            }

            for server, config in pairs(lsp_servers) do
                require("lspconfig")[server].setup(config)
            end

            -- Hover and signature help windows have rounded borders
            vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" })
            vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" })

            -- LspInfo window have rounded border
            require("lspconfig.ui.windows").default_options.border = "rounded"

            vim.api.nvim_create_autocmd("BufWritePre", {
                pattern = { "*.rs", "*.lua" },
                callback = function(_) vim.lsp.buf.format() end,
            })
            require("null-ls").setup {
                sources = {
                    require("null-ls").builtins.code_actions.gitsigns,
                    require("null-ls").builtins.diagnostics.golangci_lint,
                    require("null-ls").builtins.diagnostics.trail_space.with { disabled_filetypes = { "NvimTree" } },
                    require("null-ls").builtins.formatting.stylua,
                    require("null-ls").builtins.formatting.goimports,
                },
            }
        end,
    },
    "stevearc/oil.nvim", -- File manager like a BOSS
    "pbrisbin/vim-mkdir", -- Automatically create directory if not exists
    "fladson/vim-kitty", -- Support Kitty terminal config syntax
    "towolf/vim-helm", -- Support for helm template syntax
    "tpope/vim-surround", -- surrounding text objects
    "kevinhwang91/nvim-bqf", -- Preview quickfix list item.
    "tpope/vim-eunuch", -- Helper commands like :Rename, :Move, :Delete, :Remove, ...
    "tpope/vim-sleuth", -- Heuristically set buffer options
    "windwp/nvim-autopairs", -- Auto insert pairs like () [] {}
    {
        "lewis6991/gitsigns.nvim",
        config = function()
            require("gitsigns").setup {
                signs = {
                    add = { text = "+" },
                    change = { text = "~" },
                    delete = { text = "_" },
                    topdelete = { text = "‾" },
                    changedelete = { text = "~" },
                },
            }
        end,
    }, -- Signs next to line numbers to show git status of a line
    "tpope/vim-fugitive", -- Second best Git client ( first one is emacs magit )
    "dag/vim-fish", -- Vim fish syntax
    "jansedivy/jai.vim", -- Jai from Jonathan Blow
    {
        "akinsho/toggleterm.nvim",
        config = function()
            require("toggleterm").setup {
                size = 20,
                direction = "horizontal",
            }
        end,
    }, -- Terminal inside neovim
    {
        "folke/which-key.nvim",
        config = function()
            require("which-key").setup {
                window = {
                    border = "single",
                },
            }
        end,
    }, -- Cheat your way through keyvim.keymap.setings
    {
        "nvim-tree/nvim-tree.lua",
        config = function()
            require("nvim-tree").setup() -- Tree file explorer
        end,
    }, -- Tree file explorer
    {
        "folke/zen-mode.nvim",
        config = function() require("zen-mode").setup() end,
    },
}

-- ==========================================================================
-- ========================= Keyvim.keymap.setings ====================================
-- ==========================================================================
vim.g.mapleader = " "
-- Editing
vim.keymap.set("t", "<Esc>", "<C-\\><C-n>")
vim.keymap.set("t", "jk", "<C-\\><C-n>")
vim.keymap.set("t", "kj", "<C-\\><C-n>")
vim.keymap.set("i", "jk", "<esc>")
vim.keymap.set("i", "kj", "<esc>")
vim.keymap.set("n", "Y", "y$")
-- Splits management
vim.keymap.set("n", "<leader>v", "<cmd>vsplit<CR>", { desc = "Split vertically" })
vim.keymap.set("n", "<leader>h", "<cmd>split<CR>", { desc = "Split horizontaly" })
vim.keymap.set("n", "<Left>", "<cmd>vertical resize -10<CR>")
vim.keymap.set("n", "<Right>", "<cmd>vertical resize +10<CR>")
vim.keymap.set("n", "<C-w>=", "<cmd>wincmd =<CR>")
-- Window navigation
vim.keymap.set({ "n", "i" }, "<C-l>", "<cmd>wincmd l<CR>", { desc = "Move to split right" })
vim.keymap.set({ "n", "i" }, "<C-k>", "<cmd>wincmd k<CR>", { desc = "Move to split above" })
vim.keymap.set({ "n", "i" }, "<C-j>", "<cmd>wincmd j<CR>", { desc = "Move to split below" })
vim.keymap.set({ "n", "i" }, "<C-h>", "<cmd>wincmd h<CR>", { desc = "Move to split left" })
-- Git
vim.keymap.set("n", "<leader>g", vim.cmd.Git, { desc = "Git status" })
vim.keymap.set("n", "<leader>b", function() require("gitsigns").blame_line { full = true } end, { desc = "Git blame line" })
vim.keymap.set("n", "<leader>d", function() require("gitsigns").diffthis "~" end, { desc = "Diff current file with HEAD" })
vim.keymap.set("n", "<leader>P", function() vim.cmd.Git "push" end, { desc = "Diff current file with HEAD" })
-- Navigation
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
-- Telescope
local no_preview = { previewer = false, layout_config = { height = 0.5 } }
local dropdown = require("telescope.themes").get_dropdown
local telescope_builtin = require "telescope.builtin"
vim.keymap.set("n", "<C-p>", function() telescope_builtin.git_files(dropdown(no_preview)) end, { desc = "Telescope Git Files" })
vim.keymap.set("n", "<leader>pf", function() telescope_builtin.find_files(dropdown(no_preview)) end, { desc = "Telescope Find files" })
vim.keymap.set("n", "<leader><leader>", function() telescope_builtin.find_files(dropdown(no_preview)) end, { desc = "Telescope Find files" })
vim.keymap.set("n", "<C-f>", function() telescope_builtin.current_buffer_fuzzy_find(no_preview) end, { desc = "Current File Search" })
vim.keymap.set("n", "<leader>o", function() telescope_builtin.treesitter(dropdown(no_preview)) end, { desc = "Search Symbols In Current File" })
vim.keymap.set("n", "??", function() telescope_builtin.live_grep(no_preview) end, { desc = "Live Grep" })
vim.keymap.set("n", "Q", "<NOP>")
vim.keymap.set("n", "{", ":cprev<CR>")
vim.keymap.set("n", "}", ":cnext<CR>")
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
-- LSP
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
        vim.keymap.set("n", "gf", vim.lsp.buf.format, buffer "Format Document")
        vim.keymap.set("n", "gl", vim.diagnostic.open_float, buffer "")
        vim.keymap.set("n", "gp", vim.diagnostic.goto_prev, buffer "Next Diagnostic")
        vim.keymap.set("n", "gn", vim.diagnostic.goto_next, buffer "Previous Diagnostic")
        vim.keymap.set("n", "C", vim.lsp.buf.code_action, buffer "Code Actions")
        vim.keymap.set("n", "<C-s>", vim.lsp.buf.signature_help, buffer "Signature Help")
        vim.keymap.set("i", "<C-s>", vim.lsp.buf.signature_help, buffer "Signature Help")
        vim.keymap.set(
            "n",
            "<leader>o",
            function() require("telescope.builtin").lsp_document_symbols(dropdown(no_preview)) end,
            buffer "Document Symbols"
        )
    end,
})
vim.keymap.set({ "n", "t", "i" }, "<A-j>", vim.cmd.ToggleTerm, { desc = "ToggleTerm" }) -- Terminal
vim.keymap.set({ "n" }, "<A-b>", vim.cmd.NvimTreeToggle, { desc = "NvimTreeToggle" }) -- Tree File Explorer
