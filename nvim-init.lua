local transparent = os.getenv("NVIM_TRANSPARENT") or true
local DOTFILES_PATH = "~/.dotfiles"

vim.g.mapleader = " "

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

-- Install plugins and load.
require("lazy").setup({
    { -- AI apocalypse is here !!!!
        "supermaven-inc/supermaven-nvim",
        config = function()
            require("supermaven-nvim").setup({})
        end,
    },
    {
        "folke/snacks.nvim",
        dependencies = {
            "nvim-tree/nvim-web-devicons",
        },
        lazy = false,
        config = function()
            Snacks = require("snacks")
            Snacks.setup({
                bigfile = { enabled = true },
                indent = {
                    enabled = true,
                    animate = { enabled = false },
                    scope = { enabled = false },
                    filter = function(buf)
                        return vim.bo[buf].filetype == "yaml"
                    end,
                },
                input = { enabled = true },
                picker = {
                    enabled = true,
                    layout = {
                        preview = false,
                        layout = {
                            backdrop = false,
                            width = 0.7,
                            min_width = 80,
                            height = 0.8,
                            min_height = 3,
                            box = "vertical",
                            border = "rounded",
                            title = "{title}",
                            title_pos = "center",
                            { win = "input",   height = 1,          border = "bottom" },
                            { win = "list",    border = "none" },
                            { win = "preview", title = "{preview}", height = 0.4,     border = "top" },
                        },
                    },
                },
                notifier = { enabled = true },
                quickfile = { enabled = true },
                scope = { enabled = true },
            })
            vim.keymap.set("n", "<leader><leader>", function()
                Snacks.picker.files({})
            end, {})

            vim.keymap.set("n", "<leader>i", function()
                Snacks.picker.files({
                    prompt = "dotfiles> ",
                    cwd = DOTFILES_PATH,
                    preview = "none",
                })
            end, {})

            vim.keymap.set("n", "<leader>sd", function()
                Snacks.picker.files({ cwd = "~/.dotfiles" })
            end, {})

            vim.keymap.set("n", "<C-p>", function()
                Snacks.picker.git_files({})
            end, {})

            vim.keymap.set("n", "??", function()
                Snacks.picker.grep({ layout = "default" })
            end, {})

            vim.keymap.set("n", "<leader>o", function()
                Snacks.picker.lsp_symbols()
            end, {})

            vim.keymap.set("n", "<leader>O", function()
                Snacks.picker.lsp_workspace_symbols()
            end, {})

            vim.keymap.set("n", "<leader>h", function()
                Snacks.picker.help()
            end, {})

            vim.keymap.set("n", "<leader>b", function()
                Snacks.picker.buffers()
            end, {})

            vim.keymap.set("n", "<leader>d", function()
                Snacks.picker.diagnostics_buffer()
            end, {})

            vim.keymap.set("n", "<leader>D", function()
                Snacks.picker.diagnostics()
            end, {})

            vim.keymap.set("n", "<leader>e", function()
                Snacks.explorer()
            end, {})
        end
    },
    { -- Mason: Install lsp servers/formatters/etc.
        "williamboman/mason-lspconfig.nvim",
        opts = {},
        dependencies = {
            {
                "williamboman/mason.nvim",
                opts = { ensure_installed = { "gopls" } },
            },
        },
    },
    { -- LSP configuration
        "neovim/nvim-lspconfig",
        dependecies = {},
        config = function()
            local lspconfig = require("lspconfig")
            lspconfig.gopls.setup({})
            lspconfig.ols.setup({}) -- odin
            lspconfig.intelephense.setup({})
            lspconfig.rust_analyzer.setup({})
            lspconfig.zls.setup({})
            lspconfig.lua_ls.setup({
                settings = {
                    Lua = {
                        telemetry = { enable = false },
                        diagnostics = {
                            globals = { "vim" },
                        },
                    },
                },
            })
        end,
    },
    { -- File management done right
        "stevearc/oil.nvim",
        opts = {
            buf_options = {
                buflisted = true,
                bufhidden = "hide",
            },
        },
    },
    {
        "nvim-treesitter/nvim-treesitter",
        dependencies = {
            { "folke/ts-comments.nvim", opts = {} },
        },
        config = function()
            require("nvim-treesitter.configs").setup({
                auto_install = false,
                sync_install = false,
                ensure_installed = { "lua", "go", "gomod", "markdown", "php", "c", "cpp" },
                ignore_install = {},
                highlight = { enable = true },
                modules = {},
            })
        end,
    },
    {
        "folke/tokyonight.nvim",
        opts = { style = "moon", transparent = transparent, },
    },
    {
        "rose-pine/neovim",
        name = "rose-pine",
        opts = { dark_variant = "moon", styles = { italic = false, transparency = transparent } },
    },
    { "catppuccin/nvim", name = "catppuccin", opts = { transparent_background = transparent } },
}, {
    change_detection = { notify = false },
})

vim.cmd.colorscheme(os.getenv("NVIM_COLORSCHEME") or "rose-pine-moon") -- Colorscheme

vim.g.mapleader = " "                                                  -- <leader> key for keymaps mapped to <Space>
vim.opt.wrap = true                                                    -- Wrap long lines
vim.opt.breakindent = true                                             -- Wrapped lines have same indentation as the actual line.
vim.opt.swapfile = false                                               -- No annoying swapfiles
vim.opt.backup = false                                                 -- Disable Vim backups, we have Git :)
vim.opt.undofile = true                                                -- Save undo history
vim.opt.hlsearch = false                                               -- Highlight all matches of a search pattern.
vim.opt.incsearch = true                                               -- Match pattern while typing.
vim.opt.signcolumn = "yes"                                             -- Keep signcolumn always visible
vim.opt.splitbelow = true                                              -- How new splits are created
vim.opt.splitright = true
vim.opt.sw = 4                                                         -- TABs and indentation
vim.opt.ts = 4
vim.opt.expandtab = true
vim.g.netrw_browse_split = 0 -- minimal netrw (vim default file manager)
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.opt.guicursor = ""
vim.opt.timeoutlen = 300 -- vim update time
vim.opt.cursorline = false
vim.opt.updatetime = 250
vim.opt.termsync = false
vim.opt.number = true             -- Line numbers
vim.opt.mouse = "a"               -- Enable mouse in all modes.
vim.opt.clipboard = "unnamedplus" -- Clipboard
vim.opt.ignorecase = true         -- Case-insensitive searching UNLESS \C or capital in search
vim.opt.smartcase = true
vim.opt.completeopt = { "fuzzy", "menu", "noinsert", "noselect", "popup" }
vim.opt.inccommand = "" -- Preview all substitutions(replacements).
vim.opt.scrolloff = 10  -- Minimal number of screen lines to keep above and below the cursor.
vim.opt.laststatus = 3  -- Global statusline
vim.keymap.set("n", "Y", "y$", { desc = "Copy whole line" })
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
vim.keymap.set("n", "{", "<cmd>cprev<CR>") -- Quick fix list
vim.keymap.set("n", "}", "<cmd>cnext<CR>") -- Quickfix list
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-o>", "<C-o>zz")
vim.keymap.set("n", "<C-i>", "<C-i>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("n", "Q", "<cmd>q<CR>")
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("n", "<M-Left>", "<c-w>5>")
vim.keymap.set("n", "<M-Right>", "<c-w>5<")
vim.keymap.set("n", "<M-Up>", "<C-W>+")
vim.keymap.set("n", "<M-Down>", "<C-W>-")
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set("t", "<C-w><C-w>", function()
    vim.cmd([[ wincmd w ]])
end)
vim.keymap.set("n", "<leader>l", vim.diagnostic.open_float, { desc = "Diagnostics: Open float window" })
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Set Local list" })
vim.keymap.set("n", "<leader>g", "<cmd>LazyGit<CR>", { desc = "Lazy Git" })
vim.keymap.set({ "n", "i", "t" }, "<C-h>", "<cmd>tabprev<CR>", {})
vim.keymap.set({ "n", "i", "t" }, "<C-l>", "<cmd>tabnext<CR>", {})

vim.cmd([[ command! W :w ]])

local qflist = false
vim.keymap.set("n", "<C-q>", function()
    if qflist == true then
        qflist = not qflist
        vim.cmd([[ cclose ]])
    else
        qflist = not qflist
        vim.cmd([[ copen ]])
    end
end, { desc = "Open Quickfix list" })

if vim.fn.has("wsl") == 1 then -- Windows
    vim.g.clipboard = {
        name = "WslClipboard",
        copy = {
            ["+"] = "clip.exe",
            ["*"] = "clip.exe",
        },
        paste = {
            ["+"] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
            ["*"] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
        },
        cache_enabled = 0,
    }
end

vim.api.nvim_create_autocmd("TextYankPost", { -- Highlight yanked text
    group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
    callback = function()
        vim.highlight.on_yank()
    end,
})

-- LSP setup
vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
        local bufnr = args.buf
        vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc", { buf = bufnr })
        local map = function(mode, key, fn, desc)
            vim.keymap.set(mode, key, fn, { buffer = bufnr, desc = "LSP: " .. desc })
        end
        local references = vim.lsp.buf.references
        local implementations = vim.lsp.buf.implementation
        local has_snacks, Snacks = pcall(require, "snacks")
        if has_snacks then
            references = Snacks.picker.lsp_references
            implementations = Snacks.picker.lsp_implementations
        end

        local border = "rounded"
        map("n", "[[", function()
            vim.diagnostic.jump({ count = -1 })
        end, "Diagnostics: Next")
        map("n", "]]", function()
            vim.diagnostic.jump({ count = 1 })
        end, "Diagnostics: Previous")
        map("n", "C-]", vim.lsp.buf.definition, "[g]oto definition")
        map("n", "gd", vim.lsp.buf.definition, "[g]oto [d]efinition")
        map("n", "gD", vim.lsp.buf.declaration, "[g]oto [D]eclaration")
        map("n", "gI", implementations, "[g]oto [i]mplementation")
        map("n", "gr", references, "[g]oto [r]eferences")
        map("n", "R", vim.lsp.buf.rename, "Rename")
        map("n", "K", function()
            vim.lsp.buf.hover({ border = border })
        end, "Hover")
        map("n", "C", vim.lsp.buf.code_action, "Code Actions")
        map("n", "<leader>f", vim.lsp.buf.format, "Format")
        map({ "n", "i" }, "<C-s>", function()
            vim.lsp.buf.signature_help({ border = border })
        end, "Signature Help")
        vim.diagnostic.config({
            enabled = false,
            virtual_text = false,
            float = { border = border },
        })
        vim.keymap.set("i", "<c-space>", function()
            vim.lsp.completion.get()
        end)
        vim.keymap.set("i", "<CR>", function()
            return vim.fn.pumvisible() == 1 and "<C-y>" or "<CR>"
        end, { expr = true, noremap = true })

        vim.lsp.completion.enable(true, args.data.client_id, args.buf, { wutotrigger = false }) -- setup completion menu

        local client = vim.lsp.get_client_by_id(args.data.client_id)
        if client.supports_method("textDocument/formatting") then
            vim.api.nvim_create_autocmd("BufWritePre", {
                buffer = args.buf,
                callback = function()
                    vim.lsp.buf.format({ async = false, id = args.data.client_id })
                end,
            })
        end
    end,
})

local floating_term = { win = -1, buf = -1 }

local function toggle_floating_terminal()
    if vim.api.nvim_buf_is_valid(floating_term.buf) and vim.api.nvim_win_is_valid(floating_term.win) then
        vim.api.nvim_win_hide(floating_term.win)
        return
    end

    if not vim.api.nvim_buf_is_valid(floating_term.buf) then
        print("creating floating term buffer")
        floating_term.buf = vim.api.nvim_create_buf(false, true)
    end

    local width = math.floor(vim.o.columns * 0.8)
    local height = math.floor(vim.o.lines * 0.8)
    local row = math.floor((vim.o.lines - height) / 2)
    local col = math.floor((vim.o.columns - width) / 2)

    local win = vim.api.nvim_open_win(floating_term.buf, true, {
        relative = "editor",
        width = width,
        height = height,
        row = row,
        col = col,
        style = "minimal",
        border = "rounded",
    })

    if vim.api.nvim_get_option_value("buftype", { buf = floating_term.buf }) ~= "terminal" then
        vim.cmd.term()
    end

    vim.cmd.startinsert()

    floating_term = { buf = floating_term.buf, win = win }
end

local hsplit_terminal = { win = -1, buf = -1 }

local function toggle_hsplit_terminal()
    if vim.api.nvim_buf_is_valid(hsplit_terminal.buf) and vim.api.nvim_win_is_valid(hsplit_terminal.win) then
        vim.api.nvim_win_hide(hsplit_terminal.win)
        return
    end

    if not vim.api.nvim_buf_is_valid(hsplit_terminal.buf) then
        hsplit_terminal.buf = vim.api.nvim_create_buf(false, true)
    end

    local width = vim.o.columns
    local height = math.floor(vim.o.lines * 0.3)

    local win = vim.api.nvim_open_win(hsplit_terminal.buf, true, {
        split = "below",
        width = width,
        height = height,
    })

    if vim.api.nvim_get_option_value("buftype", { buf = hsplit_terminal.buf }) ~= "terminal" then
        vim.cmd.term()
    end

    vim.cmd.startinsert()

    hsplit_terminal = { buf = hsplit_terminal.buf, win = win }
end

local tab_terminal_state = { last_tab = -1 }

local function toggle_terminal_tab()
    local current_win = vim.api.nvim_get_current_win()
    if vim.wo[current_win].winbar == "Terminal" then
        vim.api.nvim_set_current_tabpage(tab_terminal_state.last_tab)
        return
    end
    for _, tab_id in ipairs(vim.api.nvim_list_tabpages()) do
        local win_id = vim.api.nvim_tabpage_get_win(tab_id)
        local buf_id = vim.api.nvim_win_get_buf(win_id)
        if vim.wo[win_id].winbar == "Terminal" and vim.bo[buf_id].buftype == "terminal" then
            tab_terminal_state.last_tab = vim.api.nvim_get_current_tabpage()
            vim.api.nvim_set_current_tabpage(tab_id)
            vim.cmd.startinsert()
            return
        end
    end

    tab_terminal_state.last_tab = vim.api.nvim_get_current_tabpage()
    vim.cmd.tabnew()
    local win_id = vim.api.nvim_get_current_win()
    vim.wo[win_id].winbar = "Terminal"
    vim.cmd.term()
    vim.cmd.startinsert()
end

vim.keymap.set({ "n", "t" }, "<c-j>", toggle_hsplit_terminal)
