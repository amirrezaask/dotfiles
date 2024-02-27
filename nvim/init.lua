vim.opt.errorbells = false
vim.opt.smartindent = true
vim.opt.wrap = true
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = true
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.signcolumn = "yes"
vim.opt.isfname:append("@-@")
vim.opt.shortmess:append("c") -- Don't pass messages to |ins-completion-menu|.
vim.opt.shortmess:append("I") -- No Intro message
-- How new splits are created
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.cursorline = true -- Current line highlight
-- TABs and indentation
vim.opt.sw = 4
vim.opt.ts = 4
vim.opt.expandtab = true
-- minimal netrw
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
-- vim update time
vim.opt.timeoutlen = 300
vim.opt.updatetime = 250

vim.opt.completeopt = "menu"
vim.opt.statusline = "%q%w%h%r%m%f %y %l:%c %p%%"
vim.opt.title = true
vim.opt.titlestring = '%F'
vim.opt.breakindent = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.mouse = 'a'
vim.opt.showmode = false
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.inccommand = 'split'
vim.opt.scrolloff = 10


IS_WINDOWS = vim.fn.has("win32") == 1

-- Keymaps
vim.g.mapleader =
" "                                                                                  -- <leader> key for keymaps mapped to <Space>
vim.keymap.set("n", "Y", "y$", { desc = "Copy whole line" })                         -- Make yanking act like other operations
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')                                  -- Esc should remove incsearch highlights
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

-- W is alias for w
vim.cmd [[
    command! W :w
]]

-- Highlight on Yank
vim.api.nvim_create_autocmd('TextYankPost', {
    group = vim.api.nvim_create_augroup('YankHighlight', { clear = true }),
    callback = function()
        vim.highlight.on_yank()
    end,
})


-- statusline
local mode_texts = {
    n = "Normal",
    i = "Insert",
    v = "Visual",
    c = "Complete"
}
function AmirrezaStatusLine()
    local mode = vim.api.nvim_get_mode().mode
    if mode_texts[mode] ~= nil then
        mode = mode_texts[mode]
    end

    return mode .. " " .. "%r%h%w%q%m%f %y %l:%c %p%%"
end

vim.opt.statusline = '%!v:lua.AmirrezaStatusLine()'

-- Transparency Control
TRANSPARENT = true

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

require "lazy".setup({
    { "numToStr/Comment.nvim", opts = {} }, -- [gc] to comment region/line.
    { "tpope/vim-sleuth" },                 -- Detect tabstop and shiftwidth automatically.
    {
        'lewis6991/gitsigns.nvim', -- Show git changed signs next to line numbers.
        opts = {
            signs = {
                add = { text = '+' },
                change = { text = '~' },
                delete = { text = '_' },
                topdelete = { text = '‾' },
                changedelete = { text = '~' },
            },
        },
    },
    {
        "rose-pine/neovim",
        name = "rose-pine",
        config = function()
            require "rose-pine".setup({
                styles = {
                    italic = false,
                    transparency = TRANSPARENT
                }
            })
            -- vim.cmd.colorscheme("rose-pine")
        end,
    },
    {
        "catppuccin/nvim",
        name = "catppuccin",
        config = function()
            require "catppuccin".setup({
                no_italic = false,    -- Force no italic
                no_bold = false,      -- Force no bold
                no_underline = false, -- Force no underline
                transparent_background = TRANSPARENT,
            })
            -- vim.cmd.colorscheme("catppuccin")
        end,
    },
    {
        'folke/tokyonight.nvim',
        config = function()
            require "tokyonight".setup({
                transparent = TRANSPARENT,
            })

            vim.cmd.colorscheme("tokyonight-night")
        end,
    },
    { -- Treesitter parsers.
        'nvim-treesitter/nvim-treesitter',
        build = ':TSUpdate',
        config = function()
            require('nvim-treesitter.configs').setup {
                ensure_installed = { 'lua', 'vimdoc', 'go', 'gomod' },
                auto_install = true,
                highlight = { enable = true },
                indent = { enable = true },
            }
        end,
    },
    { -- Autocomplete popup
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
    { -- Language server protocol client
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
                                library = {
                                    '${3rd}/luv/library',
                                    unpack(vim.api.nvim_get_runtime_file('', true)),
                                }
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
                    vim.lsp.buf.format()
                end,
            })
        end,
    },
    { -- Fuzzy finder
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
            local bind = function(mode, key, fn, desc)
                vim.keymap.set(mode, key, fn, { desc = "Telescope: " .. desc })
            end
            bind("n", "<C-p>", function() builtin.git_files(no_preview) end, "Git Files")

            bind("n", "<leader>i", function() builtin.find_files { cwd = vim.fn.stdpath("config") } end,
                "Edit Neovim Config")

            bind("n", "<leader>b", function() builtin.buffers(no_preview) end, "Buffers")

            bind("n", "<leader>/", function() builtin.current_buffer_fuzzy_find(no_preview) end,
                "Fuzzy find in current buffer")

            bind("n", "<leader><leader>", function() builtin.find_files(no_preview) end, "Fuzzy find")

            bind("n", "<leader>.", function() builtin.grep_string({ layout_config = { height = 0.7, width = 0.9 } }) end,
                "Grep current word")

            bind("n", "<leader>o", function() builtin.treesitter(no_preview) end, "Treesitter symbols")

            bind("n", "??", function() builtin.live_grep({ layout_config = { height = 0.9, width = 0.9 } }) end,
                "Grep in project")

            bind("n", "<leader>w", function() builtin.lsp_workspace_symbols(no_preview) end, "LSP workspace symbols")
        end,
    },



}, {})

if TRANSPARENT then
    vim.cmd [[
        hi! Normal guibg=none
        hi! NormalFloat guibg=none
    ]]
end
