--    ___         _                      ___       __
--   / _ | __ _  (_)__________ ___ ___ _/ _ | ___ / /__
--  / __ |/  ' \/ / __/ __/ -_)_ // _ `/ __ |(_-</  '_/
-- /_/ |_/_/_/_/_/_/ /_/  \__//__/\_,_/_/ |_/___/_/\_\
-- Minimal, fast configuration for neovim.

TRANSPARENT = os.getenv('NVIM_TRANSPARENT') or false
COLORSCEHEME = os.getenv('NVIM_COLORSCHEME') or "onedark"
IS_WINDOWS = vim.fn.has("win32") == 1

vim.opt.wrap = true        -- Wrap long lines
vim.opt.breakindent = true -- Wrapped lines have same indentation as the actual line.
vim.opt.swapfile = false   -- No annoying swapfiles
vim.opt.backup = false     -- Disable Vim backups, we have Git :)
vim.opt.undofile = true    -- Save undo history
vim.opt.hlsearch = false   -- Highlight all matches of a search pattern.
vim.opt.incsearch = true   -- Match pattern while typing.
vim.opt.signcolumn = "yes" -- Keep signcolumn always visible
vim.opt.splitbelow = true  -- How new splits are created
vim.opt.splitright = true
vim.opt.sw = 4             -- TABs and indentation
vim.opt.ts = 4
vim.opt.expandtab = true
vim.g.netrw_browse_split = 0 -- minimal netrw (vim default file manager)
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.opt.timeoutlen = 300 -- vim update time
vim.opt.updatetime = 250
vim.opt.termsync = false
vim.opt.number = true -- Line numbers
vim.opt.relativenumber = true
vim.opt.mouse = "a"
vim.opt.clipboard = "unnamedplus" -- Clipboard
vim.opt.ignorecase = true         -- Case-insensitive searching UNLESS \C or capital in search
vim.opt.smartcase = true
vim.opt.completeopt = { 'menu', 'noinsert' }
vim.opt.inccommand = "" -- Preview all substitutions(replacements).
vim.opt.scrolloff = 10  -- Minimal number of screen lines to keep above and below the cursor.
vim.opt.cursorline = true
vim.opt.laststatus = 3  -- Global statusline

vim.g.mapleader = " "   -- <leader> key for keymaps mapped to <Space>
vim.keymap.set("n", "Y", "y$", { desc = "Copy whole line" })
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
vim.keymap.set("n", "{", "<cmd>cprev<CR>") -- Quick fix list
vim.keymap.set("n", "}", "<cmd>cnext<CR>") -- Quickfix list
vim.keymap.set("i", "<C-Space>", "<C-x><C-o>")
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-o>", "<C-o>zz")
vim.keymap.set("n", "<C-i>", "<C-i>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("n", "<leader>v", "<cmd>vs<CR>")
vim.keymap.set("n", "<leader>s", "<cmd>sp<CR>")
vim.keymap.set("n", "Q", "<cmd>q<CR>")
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
vim.keymap.set("n", "<leader>i", "<cmd>edit $MYVIMRC<CR>")
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("n", "<M-Left>", "<c-w>5>")
vim.keymap.set("n", "<M-Right>", "<c-w>5<")
vim.keymap.set("n", "<M-Up>", "<C-W>+")
vim.keymap.set("n", "<M-Down>", "<C-W>-")
vim.keymap.set({ "n", "t", "i" }, "<C-;>", "<cmd>tabprev<CR>")
vim.keymap.set({ "n", "t", "i" }, "<C-'>", "<cmd>tabnext<CR>")
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set("t", "<C-w><C-w>", function() vim.cmd([[ wincmd w ]]) end)
vim.keymap.set({ "i" }, "<C-a>", "<C-x><C-o>") -- simpler omnifunc completion
vim.keymap.set("n", "<leader>l", vim.diagnostic.open_float, { desc = "Diagnostics: Open float window" })
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Set Local list" })
vim.keymap.set("n", "<leader>g", "<cmd>LazyGit<CR>", { desc = "Lazy Git" })
vim.cmd([[ command! W :w ]])

if vim.fn.has('wsl') == 1 then
    vim.g.clipboard = {
        name = 'WslClipboard',
        copy = {
            ['+'] = 'clip.exe',
            ['*'] = 'clip.exe',
        },
        paste = {
            ['+'] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
            ['*'] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
        },
        cache_enabled = 0,
    }
end

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
    { -- colorschemes
        "catppuccin/nvim",
        dependencies = {
            {
                'navarasu/onedark.nvim',
                opts = {
                    style = 'darker'
                }
            },
            {
                "rose-pine/neovim",
                name = 'rose-pine',
                opts = {
                    styles = {
                        bold = true,
                        italic = false,
                        transparency = TRANSPARENT,
                    }
                }
            },
            {
                "sainnhe/gruvbox-material",
                enabled = true,
                priority = 1000,
                config = function()
                    vim.g.gruvbox_material_transparent_background = 0
                    vim.g.gruvbox_material_foreground = "mix"
                    vim.g.gruvbox_material_background = "hard"
                    vim.g.gruvbox_material_ui_contrast = "high"
                    vim.g.gruvbox_material_float_style = "bright"
                    vim.g.gruvbox_material_statusline_style = "material"
                    vim.g.gruvbox_material_cursor = "auto"

                    -- vim.g.gruvbox_material_colors_override = { bg0 = '#16181A' } -- #0e1010
                    -- vim.g.gruvbox_material_better_performance = 1

                    vim.cmd.colorscheme("gruvbox-material")
                end,
            },
            {
                "folke/tokyonight.nvim",
                opts = {
                    style = 'moon',
                    transparent = TRANSPARENT,
                }
            },
        },
        name = 'catppuccin',
        opts = {
            transparent_background = TRANSPARENT,
            no_italic = true,
            background = {
                light = "latte",
                dark = "mocha",
            },
        },
    },


    -- }}}

    { -- Floating Terminal
        'akinsho/toggleterm.nvim',
        version = "*",
        opts = {
            open_mapping = [[<c-j>]],
            direction = 'float',
        }
    },

    { -- StatusLine
        'nvim-lualine/lualine.nvim',
        opts = {

            options = {
                icons_enabled = false,
            }
        }
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
            "folke/ts-comments.nvim",
        },
        config = function()
            vim.o.foldmethod = 'expr'                     -- Use expression for folding
            vim.o.foldexpr = 'nvim_treesitter#foldexpr()' -- Set Tree-sitter folding expression
            vim.o.foldenable = false                      -- Start with all folds open
            require("nvim-treesitter.configs").setup({
                ensure_installed = { "lua", "go", "gomod", "markdown", "php", "c", "cpp" },
                highlight = { enable = true },
            })

            local augroup = vim.api.nvim_create_augroup("amirreza-chcwd", {})
            vim.api.nvim_create_autocmd("BufEnter", {
                callback = function(ev)
                    local filename = ev.file
                    local start_from = vim.fs.dirname(filename)

                    local root = vim.fs.dirname(
                        vim.fs.find({ ".git", "go.mod", "package.json", "cargo.toml" },
                            { upward = true, path = start_from })[1]
                    )
                    if root ~= nil and root ~= "" then
                        local abs_path = require("plenary.path").new(root or vim.fn.getcwd()):absolute()
                        vim.fn.chdir(abs_path)
                    end
                end,
                group = augroup,
            })
        end
    },

    {

        "nvim-telescope/telescope.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
            { "nvim-telescope/telescope-fzf-native.nvim", build = 'make' },
            { 'junegunn/fzf',                             build = "./install --all" },
            "nvim-telescope/telescope-ui-select.nvim",

        },

        config = function()
            require "telescope".setup({
                defaults = {
                    file_ignore_patterns = {
                        "node_modules",
                        -- "vendor"
                    }
                }
            })
            if IS_WINDOWS == false then
                require('telescope').load_extension('fzf')
            end
            require("telescope").load_extension("ui-select")

            local telescope_keys = {
                ["<leader>p"] = { "git_files", previewer = false },
                ["<leader><leader>"] = { "find_files", previewer = false },
                ["??"] = "live_grep",
                ["<leader>h"] = { "help_tags", previewer = false },
                ["<leader>b"] = { "buffers", previewer = false },
                ["<leader>w"] = "lsp_dynamic_workspace_symbols",
                ["<leader>o"] = { "lsp_document_symbols", previewer = false },
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
                        local theme = v['theme'] or function(opts) return opts end
                        require "telescope.builtin"[v[1]](theme({
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
    }, -- Auto format

    {
        "neovim/nvim-lspconfig",
        dependencies = {
            "folke/trouble.nvim",
            "williamboman/mason.nvim",
            "williamboman/mason-lspconfig.nvim",
            {
                "j-hui/fidget.nvim",
                opts = {
                    -- options
                },
            }
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

            require("trouble").setup()

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
                    map("n", "<leader>e", ":Trouble diagnostics toggle<CR>", "Trouble Toggle")

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
            local border = {
                { "╭", "CmpBorder" },
                { "─", "CmpBorder" },
                { "╮", "CmpBorder" },
                { "│", "CmpBorder" },
                { "╯", "CmpBorder" },
                { "─", "CmpBorder" },
                { "╰", "CmpBorder" },
                { "│", "CmpBorder" },
            }
            local cmp_select = { behavior = require("cmp").SelectBehavior.Select }
            local cmp = require("cmp")
            cmp.setup({
                preselect = require("cmp.types").cmp.PreselectMode.None,
                -- completion = {
                --     autocomplete = true,
                -- },
                window = {
                    completion = {
                        border = border,
                    },
                    documentation = {
                        border = border,
                    },
                },
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
    {
        "nvim-neo-tree/neo-tree.nvim",
        branch = "v3.x",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "nvim-tree/nvim-web-devicons",
            "MunifTanjim/nui.nvim",
        },
        config = function()
            vim.keymap.set({ "n", 'i' }, "<C-e>", "<cmd>Neotree toggle<CR>", { desc = "Neotree Toggle" })
        end
    }

})

-- Quickfix list
local qflist = false
function ToggleQFList()
    if qflist == true then
        qflist = not qflist
        vim.cmd([[ cclose ]])
    else
        qflist = not qflist
        vim.cmd([[ copen ]])
    end
end

vim.keymap.set("n", "<C-q>", ToggleQFList, { desc = "Open Quickfix list" })

-- Highlight on Yank
vim.api.nvim_create_autocmd("TextYankPost", {
    group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
    callback = function()
        vim.highlight.on_yank()
    end,
})

-- Color scheme
vim.cmd.colorscheme(COLORSCEHEME)
if TRANSPARENT then
    vim.cmd [[
        hi! Normal guibg=none
    ]]
end

if vim.g.neovide then
    local font = 'JetBrainsMono Nerd Font Mono'
    local font_size = 12
    vim.o.guifont = string.format('%s:h%d', font, font_size)

    function SetFont()
        local fontfamily = ""
        local fontsize = ''

        vim.ui.input({
            prompt = "Font: ",
        }, function(selected_font)
            fontfamily = selected_font
        end)

        vim.ui.input({
            prompt = "Size: ",
        }, function(size)
            fontsize = size
        end)

        if fontfamily ~= "" and fontsize ~= "" then
            font = fontfamily
            font_size = tonumber(fontsize)
            vim.o.guifont = string.format('%s:h%d', fontfamily, fontsize)
        end
    end

    function IncFontSize()
        font_size = font_size + 1
        vim.o.guifont = string.format('%s:h%d', font, font_size)
    end

    function DecFontSize()
        font_size = font_size - 1
        vim.o.guifont = string.format('%s:h%d', font, font_size)
    end

    vim.cmd [[
        command! Font :lua SetFont()<cr>
        command! IncFont :lua IncFontSize()<CR>
        command! IncFont :lua DecFontSize()<CR>
    ]]

    vim.keymap.set({ 'n', 'i', 't', 'v' }, '<C-=>', IncFontSize)
    vim.keymap.set({ 'n', 'i', 't', 'v' }, '<C-->', DecFontSize)

    vim.g.neovide_cursor_animation_length = 0.02
    vim.g.neovide_cursor_trail_size = 0.0
    vim.g.neovide_scroll_animation_length = 0.1
    vim.g.neovide_input_macos_option_key_is_meta = 'both'
end
