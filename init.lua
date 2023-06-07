----------------------------------------------------------
---                     Options                         --
----------------------------------------------------------
vim.opt.number = true         -- Line numbers
vim.opt.relativenumber = true -- Relative line numbers
vim.opt.errorbells = false
vim.opt.smartindent = true
vim.opt.wrap = true
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = true
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.termguicolors = true
vim.opt.signcolumn = "yes"
vim.opt.isfname:append "@-@"
vim.opt.updatetime = 50
vim.opt.shortmess:append "c" -- Don't pass messages to |ins-completion-menu|.
vim.opt.shortmess:append "I" -- No Intro message
vim.opt.guicursor = ''
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
vim.opt.statusline = "%{FugitiveStatusline()}%=%m%r%h%w%q%F%=L:%l C:%c"
vim.opt.laststatus = 3
vim.g.mapleader = " "
----------------------------------------------------------
---                     Basic Keymaps                   --
----------------------------------------------------------
-- Copy/paste improvements
vim.keymap.set("n", "Y", "y$", { desc = "Copy line" })                               -- Make yanking act like other operations
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]], { desc = "Copy into clipboard" }) -- Copy to clipboard
vim.keymap.set("n", "<leader>Y", [["+Y]], { desc = "Copy line into clipboard" })
vim.keymap.set("n", "<leader>p", [["+p]], { desc = "Paste from clipboard" })
-- If I visually select words and paste from clipboard, don't replace my
-- clipboard with the selected word, instead keep my old word in the clipboard
vim.keymap.set("x", "p", "\"_dP")
-- Split windows
vim.keymap.set("n", "<leader>v", "<cmd>vsplit<CR>", { desc = "Split vertically" })
vim.keymap.set("n", "<leader>h", "<cmd>split<CR>", { desc = "Split horizontaly" })
vim.keymap.set("n", "<Left>", "<cmd>vertical resize -10<CR>")
vim.keymap.set("n", "<Right>", "<cmd>vertical resize +10<CR>")
vim.keymap.set("n", "<C-w>=", "<cmd>wincmd =<CR>")
-- Tabs
vim.keymap.set({ "n", "t" }, "<C-w><C-p>", '<cmd>tabp<cr>', { desc = "Previous Tab" })
vim.keymap.set({ "n", "t" }, "<C-w><C-n>", '<cmd>tabn<cr>', { desc = "Next Tab" })
vim.keymap.set({ "n", "t" }, "<C-w><C-t>", '<cmd>tabnew | term<cr>', { desc = "New Terminal Tab" })
vim.keymap.set({ "n", "t" }, "<C-w><C-.>", '<cmd>tabnew | term<cr>', { desc = "New Terminal Tab" })
vim.keymap.set({ "n", "t" }, "<C-w><C-q>", '<cmd>wincmd q<cr>', { desc = "Close tab" })
vim.keymap.set({ "n", "t" }, "<C-w>c", "<cmd>tabnew<CR>", { desc = "New Tab" })
-- Simpler exiting insert mode
vim.keymap.set({ "i" }, "<C-c>", "<esc>")
vim.keymap.set("t", "<Esc>", "<C-\\><C-n>")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
-- Quickfix list
vim.keymap.set({ "n" }, "{", "<cmd>cprev<CR>", { desc = "Previous quick fix list item" })
vim.keymap.set({ "n" }, "}", "<cmd>cnext<CR>", { desc = "Next quick fix list item" })
-- When moving around always have pointer centered in screen
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
-- Move lines
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("n", "Q", "<cmd>q<CR>")
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true }) -- handy when doing search in a buffer


-- Wrapped lines act as normal lines
vim.keymap.set("n", 'j', 'gj')
vim.keymap.set("n", 'k', 'gk')
-- Netrw
vim.keymap.set("n", "<leader>e", "<cmd>Ex<CR>")

-- Edit this file
vim.keymap.set("n", "<leader>i", '<cmd>edit ~/.config/nvim/init.lua<CR>', { desc = "Edit init.lua" })
----------------------------------------------------------
---                     Plugins                         --
----------------------------------------------------------
-- Installing lazy package manager
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

TRANSPARENT = true
-- Installing and configuring plugins
require "lazy".setup {
    {
        {
            'amirrezaask/gruvbuddy.nvim',
            dependencies = { 'tjdevries/colorbuddy.vim' }
        },
        {
            'projekt0n/github-nvim-theme',
            config = function()
                require "github-theme".setup()
            end,

        },
        {
            "folke/neodev.nvim",
            config = function()
                require "neodev".setup()
            end
        },
        -- Colorschemes
        {
            "ellisonleao/gruvbox.nvim",
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
        },
    },

    {
        "fatih/vim-go",
        config = function()
            vim.g['go_gopls_enabled'] = 0
            vim.g['go_code_completion_enabled'] = 0
            vim.g['go_fmt_autosave'] = 0
            vim.g['go_imports_autosave'] = 0
            vim.g['go_mod_fmt_autosave'] = 0
            vim.g['go_doc_keywordprg_enabled'] = 0
            vim.g['go_def_mapping_enabled'] = 0
            vim.g['go_textobj_enabled'] = 0
            vim.g['go_list_type'] = 'quickfix'
            vim.g['go_template_autocreate'] = false
            vim.api.nvim_create_autocmd("BufWritePre", {
                pattern = { "*.go" },
                callback = function()
                    local params = vim.lsp.util.make_range_params()
                    params.context = { only = { "source.organizeImports" } }
                    local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, 2000)
                    for _, res in pairs(result or {}) do
                        for _, r in pairs(res.result or {}) do
                            if r.edit then
                                vim.lsp.util.apply_workspace_edit(r.edit, "utf-16")
                            else
                                vim.lsp.buf.execute_command(r.command)
                            end
                        end
                    end

                    vim.lsp.buf.format()
                end
            })
        end
    },


    -- Treesitter syntax highlighting and text objects.
    {
        "nvim-treesitter/nvim-treesitter",
        dependencies = { "nvim-treesitter/nvim-treesitter-textobjects", "nvim-treesitter/playground" },
        config = function()
            require("nvim-treesitter.configs").setup {
                -- Setup treesitter text objects module + highlight
                ensure_installed = { "json", "yaml", "c", "cpp", "lua", "rust", "go", "python", "php", "ocaml", "fish" },
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


    "kevinhwang91/nvim-bqf", -- Preview quickfix list item.
    "tpope/vim-surround",    -- surrounding text objects
    "tpope/vim-abolish",     -- useful text stuff
    "windwp/nvim-autopairs", -- Auto insert pairs like () [] {}
    {
        "folke/zen-mode.nvim",
        config = function() require("zen-mode").setup() end,
    },
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
        "numToStr/Comment.nvim",
        config = function() require("Comment").setup() end,
    },

    "fladson/vim-kitty", -- Support Kitty terminal config syntax
    "towolf/vim-helm",   -- Support for helm template syntax
    "jansedivy/jai.vim", -- Jai from Jonathan Blow
    "tpope/vim-sleuth",  -- Heuristically set buffer options
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
            vim.keymap.set("n", "<leader>gd", function() require("gitsigns").diffthis "~" end,
                { desc = "Diff current file with HEAD" })
            vim.keymap.set("n", "<leader>gb", require "gitsigns".blame_line, { desc = "Git blame line" })
        end,
    }, -- Signs next to line numbers to show git status of a line
    {
        "tpope/vim-fugitive",
        config = function()
            vim.keymap.set("n", "<leader>gs", vim.cmd.Git, { desc = "Git status" })
        end,
    }, -- Second best Git client ( first one is emacs magit )

    {
        "akinsho/git-conflict.nvim",
        version = "*",
        config = function() require("git-conflict").setup {} end,
    },
    -- Autocompletion popup
    {
        "hrsh7th/nvim-cmp",
        config = function()
            local cmp_select = { behavior = require "cmp".SelectBehavior.Select }
            -- Autocompletion menu using nvim-cmp
            local capabilities = vim.lsp.protocol.make_client_capabilities()
            capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)
            local cmp = require "cmp"
            cmp.setup {
                preselect = require 'cmp.types'.cmp.PreselectMode.None,
                window = {
                    completion = cmp.config.window.bordered(),
                    documentation = cmp.config.window.bordered(),
                },
                snippet = {
                    expand = function(args) vim.fn["vsnip#anonymous"](args.body) end,
                },
                mapping = cmp.mapping.preset.insert {
                    ['<C-p>'] = cmp.mapping.select_prev_item(cmp_select),
                    ['<C-n>'] = cmp.mapping.select_next_item(cmp_select),
                    ['<C-y>'] = cmp.mapping.confirm({ select = true }),
                    ["<C-Space>"] = cmp.mapping.complete(),
                    ['<CR>'] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
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
    {
        -- LSP + Mason + Null-ls: Neovim builtin LSP client + Mason for installing langauge servers + Null-Ls to integrate non LSP tools into LSP client
        "neovim/nvim-lspconfig",
        dependencies = {
            "williamboman/mason.nvim",
        },
        config = function()
            -- TODO(amirreza): find a better more cross platform way of joining paths.
            vim.env.PATH = string.format("%s/mason/bin:", vim.fn.stdpath "data") .. vim.env.PATH
            require("mason").setup {}
            local lsp_servers = {
                ocamllsp = {
                    cmd = { string.format("%s/.opam/default/bin/%s", os.getenv("HOME"), "ocamllsp") },
                    get_language_id = function(_, ftype)
                        return ftype
                    end,
                },
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
            vim.lsp.set_log_level(0)
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
    {
        "mfussenegger/nvim-dap",
        dependencies = {
            {
                "rcarriga/nvim-dap-ui",
                config = function()
                    require "dapui".setup()
                    vim.keymap.set("n", "<leader>du", ":lua require'dapui'.toggle()<CR>")
                end
            },
            {
                "leoluz/nvim-dap-go",
                config = function()
                    require('dap-go').setup()
                end
            },
            {
                'theHamsta/nvim-dap-virtual-text',
                config = function()
                    require("nvim-dap-virtual-text").setup {}
                end
            }
        },
        config = function()
            vim.keymap.set("n", "<F5>", function()
                -- require "dapui".open()
                require "dap".continue()
            end)
            vim.keymap.set("n", "<F8>", ":lua require'dap'.step_over()<CR>")
            vim.keymap.set("n", "<F7>", ":lua require'dap'.step_into()<CR>")
            vim.keymap.set("n", "<F12>", ":lua require'dap'.step_out()<CR>")
            vim.keymap.set("n", "<leader>db", ":lua require'dap'.toggle_breakpoint()<CR>")
            vim.keymap.set("n", "<leader>dB",
                ":lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>")
            vim.keymap.set("n", "<leader>dr", ":lua require'dap'.repl.open()<CR>")

            local dap, dapui = require("dap"), require("dapui")
            dap.listeners.after.event_initialized["dapui_config"] = function()
                dapui.open()
            end
            dap.listeners.before.event_terminated["dapui_config"] = function()
                dapui.close()
            end
            dap.listeners.before.event_exited["dapui_config"] = function()
                dapui.close()
            end
        end
    },

    "stevearc/oil.nvim",  -- File manager like a BOSS
    "pbrisbin/vim-mkdir", -- Automatically create directory if not exists
    "tpope/vim-eunuch",   -- Helper commands like :Rename, :Move, :Delete, :Remove, ...
    {
        -- telescope: Fuzzy finding and searching interface
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
            }                                               -- Best fuzzy finder
            require("telescope").load_extension "fzf"       -- load fzf awesomnes into Telescope
            require("telescope").load_extension "ui-select" -- Use telescope for vim.ui.select
            local no_preview = { previewer = false, layout_config = { height = 0.6, width = 0.9 } }
            -- local dropdown = require("telescope.themes").get_dropdown
            -- local ivy = require("telescope.themes").get_ivy
            local theme = function(opts) return opts end
            local telescope_builtin = require "telescope.builtin"
            vim.keymap.set("n", "<C-p>", function() telescope_builtin.git_files(theme(no_preview)) end,
                { desc = "Telescope Git Files" })
            vim.keymap.set("n", "<leader>b", function() telescope_builtin.buffers(theme(no_preview)) end,
                { desc = "Telescope Buffers" })
            vim.keymap.set("n", "<leader><leader>", function() telescope_builtin.find_files(theme(no_preview)) end,
                { desc = "Telescope Find files" })
            vim.keymap.set("n", ",,", function() telescope_builtin.current_buffer_fuzzy_find(theme(no_preview)) end,
                { desc = "Current File Search" })
            vim.keymap.set("n", "&", function() telescope_builtin.grep_string(theme(no_preview)) end,
                { desc = "Grep for word at point" })
            vim.keymap.set(
                "n",
                "<leader>o",
                function() telescope_builtin.treesitter(theme(no_preview)) end,
                { desc = "Search Symbols In Current File" }
            )
            vim.keymap.set("n", "??", function() telescope_builtin.live_grep(theme(no_preview)) end,
                { desc = "Live Grep" })
        end,
    },
}

-- Setting the colorscheme
vim.cmd.colorscheme "github_dark_high_contrast"
