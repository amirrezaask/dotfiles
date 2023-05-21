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
-- ============================ Basic Keymaps ===============================
-- ==========================================================================
vim.g.mapleader = " "
local map = vim.keymap.set
-- Copy improvements
map("n", "Y", "y$") -- Make yanking act like other operations
map({ "n", "v" }, "<leader>y", [["+y]]) -- Copy to clipboard
map("n", "<leader>Y", [["+Y]])
-- Split windows
map("n", "<leader>v", "<cmd>vsplit<CR>", { desc = "Split vertically" })
map("n", "<leader>h", "<cmd>split<CR>", { desc = "Split horizontaly" })
map({ "n", "i" }, "<C-l>", "<cmd>wincmd l<CR>", { desc = "Move to split right" })
map({ "n", "i" }, "<C-h>", "<cmd>wincmd h<CR>", { desc = "Move to split left" })
map("n", "<Left>", "<cmd>vertical resize -10<CR>")
map("n", "<Right>", "<cmd>vertical resize +10<CR>")
map("n", "<C-w>=", "<cmd>wincmd =<CR>")
-- Simpler exiting insert mode
map({ "i" }, "<C-c>", "<esc>")
map({ "t" }, "<C-c>", "<C-\\><C-n>")
map("t", "<Esc>", "<C-\\><C-n>")
map("i", "jj", "<ESC>")
-- Quickfix list
map({ "n" }, "<C-k>", "<cmd>cprev<CR>", { desc = "Previous quick fix list item" })
map({ "n" }, "<C-j>", "<cmd>cnext<CR>", { desc = "Next quick fix list item" })
-- When moving around always have pointer centered in screen
map("n", "<C-d>", "<C-d>zz")
map("n", "<C-u>", "<C-u>zz")
map("n", "n", "nzz")
map("n", "N", "Nzz")
-- Move lines
map("v", "J", ":m '>+1<CR>gv=gv")
map("v", "K", ":m '<-2<CR>gv=gv")

map("n", "Q", "<NOP>")
map("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true }) -- handy when doing search in a buffer

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

local TRANSPARENT = true
local COLORSCHEME = "tokyonight-night"

require("lazy").setup {
    {
        "nvim-lualine/lualine.nvim",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        config = function() require("lualine").setup {} end,
    },
    {
        "amirrezaask/themes",
        config = function() vim.cmd.colorscheme(COLORSCHEME) end,
        dependencies = {
            {
                "ellisonleao/gruvbox.nvim", -- Best theme of all time
                config = function()
                    require("gruvbox").setup {
                        transparent_mode = TRANSPARENT,
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
            "amirrezaask/sitruuna.vim",
            "shaunsingh/oxocarbon.nvim",
            {
                "rebelot/kanagawa.nvim",
                config = function()
                    require("kanagawa").setup {
                        transparent = TRANSPARENT,
                    }
                end,
            },
            {
                "catppuccin/nvim",
                name = "catppuccin",
                config = function()
                    require("catppuccin").setup {
                        transparent_background = TRANSPARENT,
                    }
                end,
            },
            {
                "rose-pine/neovim",
                name = "rose-pine",
                config = function() require("rose-pine").setup { disable_background = TRANSPARENT } end,
            },
            {
                "folke/tokyonight.nvim",
                config = function() require("tokyonight").setup { transparent = TRANSPARENT } end,
            }, -- folkkkkkeeeeee
        },
    },

    -- Comment code
    {
        "numToStr/Comment.nvim",
        config = function() require("Comment").setup() end,
    },
    { -- If we want FZF for some reason
        "junegunn/fzf.vim",
        dependencies = { { "junegunn/fzf", build = ":execute fzf#install()" } },
        config = function() vim.g.fzf_preview_window = {} end,
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
            local no_preview = { previewer = false, layout_config = { height = 0.8 } }
            -- local dropdown = require("telescope.themes").get_dropdown
            local dropdown = function(opts) return opts end
            local telescope_builtin = require "telescope.builtin"
            map("n", "<C-p>", function() telescope_builtin.git_files(dropdown(no_preview)) end,
                { desc = "Telescope Git Files" })
            map("n", "<leader><leader>", function() telescope_builtin.find_files(dropdown(no_preview)) end,
                { desc = "Telescope Find files" })
            map("n", ",,", function() telescope_builtin.current_buffer_fuzzy_find(no_preview) end,
                { desc = "Current File Search" })
            map("n", "<leader>o", function() telescope_builtin.treesitter(dropdown(no_preview)) end,
                { desc = "Search Symbols In Current File" })
            map("n", "??", function() telescope_builtin.live_grep(no_preview) end, { desc = "Live Grep" })
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
                    map("n", "gd", vim.lsp.buf.definition, buffer "Goto Definition")
                    map("n", "gD", vim.lsp.buf.declaration, buffer "Goto Declaration")
                    map("n", "gi", vim.lsp.buf.implementation, buffer "Goto Implementation")
                    map("n", "gr", vim.lsp.buf.references, buffer "Goto References")
                    map("n", "R", vim.lsp.buf.rename, buffer "Rename")
                    map("n", "K", vim.lsp.buf.hover, buffer "Hover")
                    map("n", "gl", vim.diagnostic.open_float, buffer "")
                    map("n", "gp", vim.diagnostic.goto_prev, buffer "Next Diagnostic")
                    map("n", "gn", vim.diagnostic.goto_next, buffer "Previous Diagnostic")
                    map("n", "C", vim.lsp.buf.code_action, buffer "Code Actions")
                    map("n", "<C-s>", vim.lsp.buf.signature_help, buffer "Signature Help")
                    map("i", "<C-s>", vim.lsp.buf.signature_help, buffer "Signature Help")
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
    "stevearc/oil.nvim", -- File manager like a BOSS
    { "fatih/vim-go", config = function() vim.g.go_template_autocreate = false end },
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
            map("n", "<leader>b", function() require("gitsigns").blame_line { full = true } end,
                { desc = "Git blame line" })
            map("n", "<leader>d", function() require("gitsigns").diffthis "~" end,
                { desc = "Diff current file with HEAD" })
        end,
    }, -- Signs next to line numbers to show git status of a line
    {
        "tpope/vim-fugitive",
        config = function()
            vim.api.nvim_create_user_command("Gp", function() vim.cmd.Git "push" end, {})

            map("n", "<leader>P", function() vim.cmd.Git "push" end, { desc = "Git Push" })
            map("n", "<leader>g", vim.cmd.Git, { desc = "Git status" })
        end,
    }, -- Second best Git client ( first one is emacs magit )

    {
        "akinsho/git-conflict.nvim",
        version = "*",
        config = function() require("git-conflict").setup {} end,
    },
    "dag/vim-fish", -- Vim fish syntax
    "jansedivy/jai.vim", -- Jai from Jonathan Blow
    {
        "folke/which-key.nvim",
        config = function()
            require("which-key").setup {
                window = {
                    border = "single",
                },
            }
        end,
    }, -- Cheat your way through keymapings
    {
        "folke/zen-mode.nvim",
        config = function() require("zen-mode").setup() end,
    },
}
