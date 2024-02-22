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
vim.opt.isfname:append("@-@")
vim.opt.updatetime = 50
vim.opt.shortmess:append("c") -- Don't pass messages to |ins-completion-menu|.
vim.opt.shortmess:append("I") -- No Intro message
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.cursorline = false
vim.opt.sw = 4
vim.opt.ts = 4
vim.opt.expandtab = true
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.opt.timeoutlen = 300
vim.opt.completeopt = "menu"
vim.opt.statusline = "%q%w%h%r%m%f %y %l:%c %p%%"
vim.opt.ignorecase = true
vim.opt.title = true
vim.opt.titlestring = '%F'
vim.opt.cursorline = true
vim.opt.breakindent = true

-- Keymaps
vim.g.mapleader =
" "                                                                                  -- <leader> key for keymaps mapped to <Space>
vim.keymap.set("n", "Y", "y$", { desc = "Copy whole line" })                         -- Make yanking act like other operations
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]], { desc = "Copy into clipboard" }) -- Copy to clipboard
vim.keymap.set("n", "<leader>Y", [["+Y]], { desc = "Copy line into clipboard" })
vim.keymap.set("n", "<leader>p", [["+p]], { desc = "Paste from clipboard" })
-- If I visually select words and paste from clipboard, don't replace my
-- clipboard with the selected word, instead keep my old word in the clipboard
vim.keymap.set("x", "p", '"_dP')
-- Simpler exiting insert mode
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
-- splits
vim.keymap.set("n", "<leader>k", ":vsplit<cr>")
vim.keymap.set("n", "<leader>j", ":split<cr>")
-- Quickfix list
vim.keymap.set({ "n" }, "<C-[>", "<cmd>cprev<CR>", { desc = "Previous quick fix list item" })
vim.keymap.set({ "n" }, "<C-]>", "<cmd>cnext<CR>", { desc = "Next quick fix list item" })
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

vim.keymap.set({ "n" }, "<C-q>", ToggleQFList, { desc = "Open Quickfix list" })
vim.keymap.set({ "i" }, "<C-Space>", "<C-x><C-o>")
-- When moving around always have cursor centered in screen
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-o>", "<C-o>zz")
vim.keymap.set("n", "<C-i>", "<C-i>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("n", "Q", "<cmd>q<CR>")
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true }) -- handy when doing search in a buffer

-- Wrapped lines act as normal lines
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")

-- Terminal and Tabs
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set({ "i", "n", "t" }, "<C-k>", "<cmd>tabnext<CR>")
vim.keymap.set({ "i", "n", "t" }, "<C-j>", "<cmd>tabprev<CR>")
vim.keymap.set("i", "<C-a>", "<C-x><C-o>") -- simpler omnifunc completion

local is_windows = vim.fn.has("win32") == 1

-- Neovide GUI
-- recommended especially in windows environment, much better than windows terminal.
if vim.g.neovide then
    local font_family = "Jetbrains Mono"
    local font_size = 15
    vim.g.neovide_scroll_animation_length = 0.00
    vim.g.neovide_cursor_animation_length = 0.00
    vim.g.neovide_cursor_vfx_mode = ""

    function Font(font, size)
        font_family = font
        font_size = size
        vim.opt.guifont = string.format("%s:h%d", font, size)
    end

    function FontSizeInc()
        font_size = 1 + font_size
        Font(font_family, font_size)
    end

    function FontSizeDec()
        font_size = font_size - 1
        Font(font_family, font_size)
    end

    function FontSize(size)
        font_size = size
        Font(font_family, font_size)
    end

    vim.api.nvim_create_user_command("FontSizeInc", function(_)
        FontSizeInc()
    end, {})

    vim.api.nvim_create_user_command("FontSizeDec", function(_)
        FontSizeDec()
    end, {})

    vim.api.nvim_create_user_command("FontSize", function(opts)
        FontSize(tonumber(opts.fargs[1]))
    end, {nargs = 1})


    vim.keymap.set({ "n", "i", "v", "x", "t" }, "<C-=>", FontSizeInc, {})
    vim.keymap.set({ "n", "i", "v", "x", "t" }, "<C-->", FontSizeDec, {})

    vim.api.nvim_create_user_command("Font", function(opts)
        local splitted = vim.split(opts.args, ":")
        if #splitted < 2 then
            error("Font command input should be in [FontName]:[FontSize] format")
        end
        Font(splitted[1], splitted[2])
    end, { nargs = "*" })

    Font("Jetbrains Mono", 16)
end

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)


require("lazy").setup({
        -- Colorschemes
        {
            'folke/tokyonight.nvim'
        },
        {
            "ellisonleao/gruvbox.nvim",
            opts = {
                contrast = 'hard',
                italic = {
                    strings = false,
                    emphasis = false,
                    comments = false,
                    operators = false,
                    folds = false,
                }
            }
        },
        { -- Theme inspired by Atom
            'navarasu/onedark.nvim',
            config = function()
                require('onedark').setup {
                    -- Set a style preset. 'dark' is default.
                    style = 'dark', -- dark, darker, cool, deep, warm, warmer, light
                }
            end,
        },
        { "tpope/vim-abolish" },                    -- useful text stuff
        { "numToStr/Comment.nvim", opts = {} }, -- Comment stuff like a boss
        { "tpope/vim-sleuth" },                     -- set buffer options heuristically
        {
            'lukas-reineke/indent-blankline.nvim',
            main = 'ibl',
            opts = {},
        },

        { "tpope/vim-fugitive" }, -- Git

        {
            "hrsh7th/nvim-cmp",
            dependencies = {
                'L3MON4D3/LuaSnip',
                'saadparwaiz1/cmp_luasnip',
                "hrsh7th/cmp-nvim-lsp",
                "hrsh7th/cmp-path",
                "hrsh7th/cmp-buffer",
            },
            config = function()
                local cmp_select = { behavior = require("cmp").SelectBehavior.Select }
                local capabilities = vim.lsp.protocol.make_client_capabilities()
                capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)
                local cmp = require("cmp")
                cmp.setup({
                    preselect = require("cmp.types").cmp.PreselectMode.None,
                    window = {
                        completion = cmp.config.window.bordered(),
                        documentation = cmp.config.window.bordered(),
                    },
                    snippet = {
                        expand = function(args)
                            require('luasnip').lsp_expand(args.body)
                        end,
                    },
                    mapping = cmp.mapping.preset.insert({
                        ["<C-p>"] = cmp.mapping.select_prev_item(cmp_select),
                        ["<C-n>"] = cmp.mapping.select_next_item(cmp_select),
                        ["<C-y>"] = cmp.mapping.confirm({ select = true }),
                        ["<C-Space>"] = cmp.mapping.complete(),
                        ["<CR>"] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
                    }),
                    sources = {
                        { name = "nvim_lsp" },
                        { name = "buffer" },
                        { name = "path" },
                    },
                })
            end,
        },
        {
            "neovim/nvim-lspconfig",
            dependencies = {
                {
                    "williamboman/mason.nvim",
                    config = function()
                        local function get_path_sep()
                            if is_windows then
                                return "\\"
                            else
                                return "/"
                            end
                        end

                        local sep = get_path_sep()

                        if is_windows then
                            vim.env.PATH = string.format("%s%smason%sbin;", (vim.fn.stdpath("data")), sep, sep) ..
                                vim.env.PATH
                        else
                            vim.env.PATH = string.format("%s%smason%sbin:", (vim.fn.stdpath("data")), sep, sep) ..
                                vim.env.PATH
                        end
                        require("mason").setup({})
                    end
                }
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
                        local buffer = function(desc)
                            return { buffer = bufnr, desc = desc }
                        end
                        vim.keymap.set("n", "gd", vim.lsp.buf.definition, buffer("Goto Definition"))
                        vim.keymap.set("n", "gD", vim.lsp.buf.declaration, buffer("Goto Declaration"))
                        vim.keymap.set("n", "gi", vim.lsp.buf.implementation, buffer("Goto Implementation"))
                        vim.keymap.set("n", "gr", vim.lsp.buf.references, buffer("Goto References"))
                        vim.keymap.set("n", "R", vim.lsp.buf.rename, buffer("Rename"))
                        vim.keymap.set("n", "K", vim.lsp.buf.hover, buffer("Hover"))
                        vim.keymap.set("n", "<leader>f", vim.lsp.buf.format, buffer("Format"))
                        vim.keymap.set("n", "gl", vim.diagnostic.open_float, buffer(""))
                        vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, buffer("Next Diagnostic"))
                        vim.keymap.set("n", "]d", vim.diagnostic.goto_next, buffer("Previous Diagnostic"))
                        vim.keymap.set("n", "C", vim.lsp.buf.code_action, buffer("Code Actions"))
                        vim.keymap.set("n", "<C-s>", vim.lsp.buf.signature_help, buffer("Signature Help"))
                        vim.keymap.set("i", "<C-s>", vim.lsp.buf.signature_help, buffer("Signature Help"))
                        vim.diagnostic.config({ virtual_text = false })
                    end,
                })
            end,

        },
        {
            "nvim-telescope/telescope.nvim",
            dependencies = {
                "nvim-lua/plenary.nvim",
                { "nvim-telescope/telescope-fzf-native.nvim", build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build" },
                "nvim-telescope/telescope-ui-select.nvim",
            },
            config = function()
                require("telescope").load_extension("ui-select") -- Use telescope for vim.ui.select
                local builtin = require("telescope.builtin")
                local no_preview = { previewer = false }
                local config_cwd = "~/.config/nvim/"
                if vim.fn.has("win32") == 1 then
                    config_cwd = "~/AppData/Local/nvim/"
                end
                vim.keymap.set("n", "<C-p>", function() builtin.git_files(no_preview) end)
                vim.keymap.set("n", "<leader>b", function() builtin.buffers(no_preview) end)
                vim.keymap.set("n", "<leader><leader>", function() builtin.find_files(no_preview) end)
                vim.keymap.set("n", "<leader>ff", function() builtin.find_files(no_preview) end)
                vim.keymap.set("n", "<leader>fc",
                    function() builtin.find_files({ cwd = config_cwd, previewer = false }) end)
                vim.keymap.set("n", "<leader>.",
                    function() builtin.grep_string({ layout_config = { height = 0.7, width = 0.9 } }) end)
                vim.keymap.set("n", "<leader>o", function() builtin.treesitter(no_preview) end)
                vim.keymap.set("n", "??",
                    function() builtin.live_grep({ layout_config = { height = 0.9, width = 0.9 } }) end)
                vim.keymap.set("n", "<leader>w", function() builtin.lsp_workspace_symbols(no_preview) end)
            end,
        },
        {
            "nvim-treesitter/nvim-treesitter",
            dependencies = {
                "nvim-treesitter/nvim-treesitter-textobjects",
                "nvim-treesitter/playground",
            },
            config = function()
                require("nvim-treesitter.configs").setup({
                    sync_install = false,
                    auto_install = true,
                    ignore_install = {},
                    modules = {},
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
                })
            end,
        }
    },
    {
        change_detection = {
            enabled = true,
            notify = false, -- get a notification when changes are found
        },

    })

-- Golang autoformat
vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*.go",
    callback = function()
        vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } }, apply = true })
        vim.lsp.buf.format()
    end,
})

-- Edit this configuration file
THIS_FILE = debug.getinfo(1, 'S').short_src
vim.keymap.set("n", "<leader>i", string.format(":e %s<cr>", THIS_FILE))

-- vim.cmd.colorscheme("gruvbox")
vim.cmd.colorscheme("onedark")
