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
vim.keymap.set("n", "<Left>", "<cmd>vertical resize -10<CR>")
vim.keymap.set("n", "<Right>", "<cmd>vertical resize +10<CR>")
vim.keymap.set("n", "<C-w>=", "<cmd>wincmd =<CR>")
-- Simpler exiting insert mode
vim.keymap.set("i", "<C-c>", "<esc>")
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

local function open_term()
    vim.cmd [[ tabnew | term ]]
end

-- terminal emulator
vim.keymap.set('t', '<esc>', [[<C-\><C-n>]])
vim.keymap.set({ "n", "t" }, "<C-k>", '<cmd>tabnext<CR>')
vim.keymap.set({ "n", "t" }, "<C-j>", '<cmd>tabprev<CR>')
vim.keymap.set({ "n", "t" }, "<A-w>", "<cmd>tabclose<CR>")
vim.keymap.set({ "n", "t" }, "<C-,>", '<cmd>tabnew<CR>')
vim.keymap.set("n", "<C-`>", open_term)

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

local function get_path_sep()
    if vim.fn.has('win32') == 1 then
        return '\\'
    else
        return "/"
    end
end
vim.opt.rtp:prepend(lazypath)

TRANSPARENT = false
COLORSCHEME = "catppuccin-mocha"
-- Installing and configuring plugins
require "lazy".setup {
    -- Colorschemes
    { "rose-pine/neovim",        name = "rose-pine" },
    { "folke/tokyonight.nvim", },
    { "ellisonleao/gruvbox.nvim" },
    { "catppuccin/nvim",         name = "catppuccin" },
    {
        "sainnhe/sonokai",
        config = function()
            vim.g.sonokai_style = "andromeda"
            vim.g.sonokai_transparent_background = TRANSPARENT
        end
    },
    {
      "folke/noice.nvim",
      dependencies = {
        "MunifTanjim/nui.nvim",
         "rcarriga/nvim-notify",
        },
        config = function ()
        require("noice").setup({
          lsp = {
            -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
            override = {
              ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
              ["vim.lsp.util.stylize_markdown"] = true,
              ["cmp.entry.get_documentation"] = true,
            },
          },
          presets = {
            bottom_search = true, -- use a classic bottom cmdline for search
            command_palette = true, -- position the cmdline and popupmenu together
            long_message_to_split = true, -- long messages will be sent to a split
            inc_rename = false, -- enables an input dialog for inc-rename.nvim
            lsp_doc_border = false, -- add a border to hover docs and signature help
          },
        })
        end
    },
    {
        'nvim-lualine/lualine.nvim',
        opts = {},
        dependencies = { 'nvim-tree/nvim-web-devicons' }
    },
    -- Treesitter syntax highlighting and text objects.
    {
        "nvim-treesitter/nvim-treesitter",
        dependencies = { "nvim-treesitter/nvim-treesitter-textobjects", "nvim-treesitter/playground",
            'nvim-treesitter/nvim-treesitter-context' },
            config=function()
                require("nvim-treesitter.configs").setup {
                    -- Setup treesitter text objects module + highlight
                    ensure_installed = {
                        "json",
                        "yaml",
                        "c",
                        "cpp",
                        "lua",
                        "rust",
                        "go",
                        "python",
                        "php",
                        "ocaml",
                        "fish",
                        "http"
                    },
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
                require 'treesitter-context'.setup { enable = true }
            end
    },
    -- Editor stuff
    "tpope/vim-surround", -- surrounding text objects
    "tpope/vim-abolish",  -- useful text stuff
    { "numToStr/Comment.nvim", opts = {} }, -- Comment stuff like a boss
    "tpope/vim-fugitive",
    "fladson/vim-kitty", -- Support Kitty terminal config syntax
    "towolf/vim-helm",   -- Support for helm template syntax
    "jansedivy/jai.vim", -- Jai from Jonathan Blow
    "tpope/vim-sleuth",  -- Heuristically set buffer options
    {
        "lewis6991/gitsigns.nvim",
        opts = {
            signs = {
                add = { text = "+" },
                change = { text = "~" },
                delete = { text = "_" },
                topdelete = { text = "‾" },
                changedelete = { text = "~" },
            },
        }
    },
    -- Autocompletion popup
    {
        "hrsh7th/nvim-cmp",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/cmp-vsnip",
            "hrsh7th/vim-vsnip",
            "hrsh7th/cmp-path",
            "hrsh7th/cmp-buffer",
        },
        config = function()
            local cmp_select = { behavior = require "cmp".SelectBehavior.Select }
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


        end
    },
    -- Language server
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            "williamboman/mason.nvim",
            { 'j-hui/fidget.nvim', tag = 'legacy' },
        },

        config = function()
            local sep = get_path_sep()
            vim.env.PATH = string.format("%s%smason%sbin:", (vim.fn.stdpath "data"), sep, sep) .. vim.env.PATH
            require "fidget".setup {}
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
            vim.diagnostic.config { virtual_text = true }

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
                    vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, buffer "Next Diagnostic")
                    vim.keymap.set("n", "]d", vim.diagnostic.goto_next, buffer "Previous Diagnostic")
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
        end
    },

    -- Golang
    { "fatih/vim-go", config = function()
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
    end},
    {
        "mfussenegger/nvim-dap",
        config = function()
            require "dapui".setup {
                layouts = {
                    {
                        elements = {
                            {
                                id = "watches",
                                size = 1
                            },
                        },
                        position = "bottom",
                        size = 10
                    }
                }
            }
            vim.keymap.set("n", "<F2>", ":lua require'dapui'.toggle()<CR>")
            vim.keymap.set("n", "<F5>", function() require "dap".continue() end)
            vim.keymap.set("n", "<F10>", ":lua require'dap'.step_over()<CR>")
            vim.keymap.set("n", "<F11>", ":lua require'dap'.step_into()<CR>")
            vim.keymap.set("n", "<F12>", ":lua require'dap'.step_out()<CR>")
            vim.keymap.set("n", "<F1>", ":lua require'dap'.toggle_breakpoint()<CR>")
            vim.keymap.set("n", "<F9>", ":lua require'dap'.repl.open()<CR>")

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
        end,
        dependencies = {
            "rcarriga/nvim-dap-ui",
            {
                "leoluz/nvim-dap-go",
                opts = {
                    dap_configurations = {
                        {
                            type = 'go',
                            name = 'Mabna: API',
                            request = 'launch',
                            program = './cmd/api'
                        },
                        {
                            type = 'go',
                            name = 'Mabna: Service',
                            request = 'launch',
                            program = './cmd/service'
                        },

                    }
                }
            },
            { 'theHamsta/nvim-dap-virtual-text', opts = {} },
        },
    },

    "pbrisbin/vim-mkdir", -- Automatically create directory if not exists
    "tpope/vim-eunuch",   -- Helper commands like :Rename, :Move, :Delete, :Remove, ...
    {
        "nvim-telescope/telescope.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
            { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
            "nvim-telescope/telescope-ui-select.nvim",
        },
        config = function()
            require("telescope").setup {
                defaults = {
                    sorting_strategy = "ascending",
                    layout_strategy = 'horizontal',
                    layout_config = {
                        horizontal = {
                            preview_cutoff = 180,
                            prompt_position = "top",
                            height = 0.6,
                            width = 0.7,
                            preview_width = 0.7,
                        }
                    },

                },
                extensions = {
                    ["ui-select"] = {
                        require("telescope.themes").get_dropdown {},
                    },
                },
            }

            require("telescope").load_extension "fzf"       -- load fzf awesomnes into Telescope
            require("telescope").load_extension "ui-select" -- Use telescope for vim.ui.select
            local telescope_builtin = require "telescope.builtin"
            local no_preview = { previewer = false }
            local dropdown = require "telescope.themes".get_dropdown

            local smart_file_picker = function()
                if vim.fn['fugitive#Head']() == "" then
                    return telescope_builtin.find_files(dropdown {
                        layout_config = { height = 0.5 },
                        previewer = false
                    })
                else
                    return telescope_builtin.git_files(dropdown({
                        layout_config = { height = 0.5 },
                        previewer = false,
                    }))
                end
            end
            vim.keymap.set("n", "<C-p>", function() telescope_builtin.git_files(dropdown(no_preview)) end, { desc = "Git Files" })
            vim.keymap.set("n", "<leader>b", function() telescope_builtin.buffers(dropdown(no_preview)) end,
                { desc = "Telescope Buffers" })
            vim.keymap.set("n", "<leader><leader>", smart_file_picker, { desc = "Smart File Picker" })
            vim.keymap.set("n", "<leader>ff", smart_file_picker, { desc = "Find Files" })
            vim.keymap.set("n", "<leader>w", telescope_builtin.grep_string, { desc = "Grep for word at point" })
            vim.keymap.set("n", "<leader>o", telescope_builtin.treesitter, { desc = "Search Symbols In Current File" })
            vim.keymap.set("n", "??", telescope_builtin.live_grep, { desc = "Live Grep" })
        end
    },
}

-- Setting the colorscheme
vim.cmd.colorscheme(COLORSCHEME)

if TRANSPARENT then
    vim.api.nvim_set_hl(0, 'Normal', { bg = nil })
    vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
end
