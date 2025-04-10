local COLORSCHEME = vim.env.NVIM_COLORSCHEME or 'tokyonight-night'
local TRANSPARENT = vim.env.NVIM_TRANSPARENT == true or false
local FUZZY_FINDER = vim.env.NVIM_FUZZY_FINDER or 'snacks' --: snacks | nvim-find
local INDENT_LINES = vim.env.NVIM_INDENT_LINES == true or false

function printf(...)
    local args = { ... }
    print(string.format(...))
end

function RELOAD(module)
    package.loaded[module] = nil
    return require(module)
end

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

vim.g.mapleader = " " -- <leader> key for keymaps mapped to <Space>
require("lazy").setup {
    { "stevearc/conform.nvim",
        opts = {
            format_on_save = function()
                if vim.tbl_contains({ "php" }, vim.bo.filetype) then
                    return false
                end
                return {
                    timeout_ms = 500,
                    lsp_format = "fallback",
                }
            end,

            formatters_by_ft = {
                -- lua = { "stylua", lsp_format = "fallback" },
                go = { "goimports", "gofmt" },
            },
        }
    },
    { "neovim/nvim-lspconfig", config = function()
        -- LSP setup
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
        vim.api.nvim_create_autocmd("LspAttach", {
            callback = function(args)
                local bufnr = args.buf
                vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc", { buf = bufnr })
                local map = function(mode, key, fn, desc)
                    vim.keymap.set(mode, key, fn, { buffer = bufnr, desc = "LSP: " .. desc })
                end
                local references = vim.lsp.buf.references
                local implementations = vim.lsp.buf.implementation
                if FUZZY_FINDER == 'snacks' then
                    references = P.lsp_references
                    implementations = P.lsp_implementations
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
                map("n", "gi", implementations, "[g]oto [i]mplementation")
                map("n", "gr", references, "[g]oto [r]eferences")
                map("n", "R", vim.lsp.buf.rename, "Rename")
                map("n", "K", function()
                    vim.lsp.buf.hover({ border = border })
                end, "Hover")
                map("n", "C", vim.lsp.buf.code_action, "Code Actions")
                map({ "n", "i" }, "<C-s>", function()
                    vim.lsp.buf.signature_help({ border = border })
                end, "Signature Help")
                map("n", "<leader>l", vim.diagnostic.open_float, "Diagnostics: Open float window")
                map("n", "<leader>q", vim.diagnostic.setloclist, "Set Local list")
                vim.diagnostic.config({
                    enabled = true,
                    virtual_text = false,
                    float = { border = border },
                })

                vim.keymap.set("i", "<CR>", function()
                    return vim.fn.pumvisible() == 1 and "<C-y>" or "<CR>"
                end, { expr = true, noremap = true })

                vim.lsp.completion.enable(true, args.data.client_id, args.buf, { wutotrigger = false }) -- setup completion menu
            end,
        })
    end
    },
    {
        "williamboman/mason.nvim",
        config = function()
            require("mason").setup({ ensure_installed = { "gopls" } })
            local process_path = os.getenv("PATH")
            vim.fn.setenv("PATH", process_path .. ":" .. vim.fn.stdpath("data") .. "/mason/bin")
        end,
    },
    {
        "saghen/blink.cmp",
        version = '1.*',
        opts = {
            keymap = { preset = "enter" },
            cmdline = { enabled = false },
        }
    },
    {
        "nvim-treesitter/nvim-treesitter",
        config = function()
            require('nvim-treesitter.configs').setup {
                auto_install = false,
                sync_install = false,
                ensure_installed = { "lua", "go", "gomod", "markdown", "php", "c", "cpp" },
                ignore_install = {},
                highlight = { enable = true },
                modules = {},
            }
        end
    },
    { "folke/ts-comments.nvim",        opts = {} },
    { "folke/snacks.nvim", dependencies = { "nvim-tree/nvim-web-devicons" }, config = function()
        Snacks = require("snacks")
        Snacks.setup {
            bigfile = { enabled = true },
            picker = {
                enabled = true,
            },
            indent = { enabled = INDENT_LINES }
        }
        P = require("snacks").picker
        if FUZZY_FINDER == 'snacks' then
            vim.keymap.set("n", "<leader><leader>", function() P.files { layout = { preview = false } } end, {})
            vim.keymap.set("n", "<leader>ff", function() P.files { layout = { preview = false } } end, {})
            vim.keymap.set("n", "<C-p>", function() P.git_files { layout = { preview = false } } end, {})
            vim.keymap.set("n", "<leader>fg", function() P.git_files { layout = { preview = false } } end, {})
            vim.keymap.set("n", "<leader>fd",
                function() P.files { cwd = "~/.dotfiles", layout = { preview = false } } end, {})
            vim.keymap.set("n", "??", P.grep, {})
            vim.keymap.set("n", "<leader>fb", P.buffers, {})
            vim.keymap.set("n", "<leader>h", P.help, {})
            vim.keymap.set("n", "<leader>d", P.diagnostics_buffer, {})
            vim.keymap.set("n", "<leader>D", P.diagnostics, {})
            vim.keymap.set("n", "<leader>o", P.lsp_symbols, {})
            vim.keymap.set("n", "<leader>O", P.lsp_workspace_symbols, {})
        end
    end
    },
    "folke/tokyonight.nvim",
    { "rose-pine/neovim",              name = "rose-pine" },
    { "catppuccin/nvim",               name = "catppuccin" },

    { "amirrezaask/nvim-blue.lua",     dir = '~/src/nvim-blue.lua' },
    { "amirrezaask/nvim-sitruuna.lua", dir = '~/src/nvim-sitruuna.lua' },
    { "amirrezaask/nvim-terminal.lua", dir = '~/src/nvim-terminal.lua' },
    { "amirrezaask/nvim-find.lua", dir = '~/src/nvim-find.lua', config = function()
        F = require("find")
        if FUZZY_FINDER == 'nvim-find' then
            vim.keymap.set("n", "<leader><leader>", F.files, {})
            vim.keymap.set("n", "<leader>ff", F.files, {})
            vim.keymap.set("n", "<C-p>", F.git_files, {})
            vim.keymap.set("n", "<leader>fg", F.git_files, {})
            vim.keymap.set("n", "<leader>fd", function() F.files { path = "~/.dotfiles" } end, {})
            vim.keymap.set("n", "??", F.ripgrep_fuzzy, {})
            vim.keymap.set("n", "<leader>fb", F.buffers, {})
            vim.keymap.set("n", "<leader>h", F.helptags, {})
            vim.keymap.set("n", "<leader>d", function() F.diagnostics({ buf = vim.api.nvim_get_current_buf() }) end, {})
            vim.keymap.set("n", "<leader>D", F.diagnostics, {})
            vim.keymap.set("n", "<leader>o", F.lsp_document_symbols, {})
            vim.keymap.set("n", "<leader>O", F.lsp_workspace_symbols, {})
        end
    end },

}


function Transparent()
    vim.cmd [[
        hi! Normal      guibg=none
        hi! NormalNC    guibg=none
        hi! NormalFloat guibg=none
        hi! FloatBorder guibg=none
        hi! FloatTitle  guibg=none
        hi! FloatFooter guibg=none
        hi! SignColumn  guibg=none
        hi! LineNr      guibg=none
    ]]
end

vim.cmd.colorscheme(COLORSCHEME)
if TRANSPARENT then Transparent() end

vim.opt.wrap = true        -- Wrap long lines
vim.opt.breakindent = true -- Wrapped lines have same indentation as the actual line.
vim.opt.swapfile = false   -- No annoying swapfiles
vim.opt.backup = false     -- Disable Vim backups, we have Git :)
vim.opt.undofile = true    -- Save undo history
vim.opt.hlsearch = false   -- Highlight all matches of a search pattern.
vim.opt.incsearch = true   -- Match pattern while typing.
vim.opt.signcolumn = "yes" -- Keep signcolumn always visible
vim.opt.cursorline = true  -- Highlight current line with hl defined as *hl-CursorLine*
vim.opt.splitbelow = true  -- How new splits are created
vim.opt.splitright = true  -- SAME
vim.opt.showmode = false   -- don't show --INSERT-- in command line.
vim.opt.sw = 4             -- TABs and indentation
vim.opt.ts = 4
vim.opt.expandtab = true
vim.g.netrw_browse_split = 0      -- minimal netrw (vim default file manager)
vim.g.netrw_banner = 0            -- no banner for netrw
vim.g.netrw_winsize = 25
vim.opt.guicursor = ""            -- I don't want my cursor shape change with mode changes.
vim.opt.timeoutlen = 300          -- Time vim waits for a key sequence to finish.
vim.opt.updatetime = 250          -- Milliseconds to wait for CursorHold autocmds to fire.
vim.opt.number = true             -- Line numbers
vim.opt.mouse = "a"               -- Enable mouse in all modes.
vim.opt.clipboard = "unnamedplus" -- Clipboard
vim.opt.ignorecase = true         -- Search has case insensitive by default, but if pattern has some upper case letters, it will be case sensitive.
vim.opt.smartcase = true          -- Search has case insensitive by default, but if pattern has some upper case letters, it will be case sensitive.
vim.opt.completeopt = { "fuzzy", "menu", "noinsert", "noselect", "popup" }
vim.opt.inccommand = ""           -- Preview all substitutions(replacements).
vim.opt.scrolloff = 10            -- Minimal number of screen lines to keep above and below the cursor.

function StatusLine()
    ---@type string
    local mode = vim.fn.mode()
    if mode == "n" then
        mode = "NORMAL"
    elseif mode == "i" then
        mode = "INSERT"
    elseif mode == "R" then
        mode = "REPLACE"
    elseif mode == "v" then
        mode = "VISUAL"
    elseif mode == "V" then
        mode = "V-LINE"
    elseif mode == "c" then
        mode = "COMMAND"
    elseif mode == "s" then
        mode = "SELECT"
    elseif mode == "S" then
        mode = "S-LINE"
    elseif mode == "t" then
        mode = "TERMINAL"
    end

    return "%l:%c %m%r%h%w%f" .. "%=" .. mode .. " %y"
end

vim.opt.statusline = "%!v:lua.StatusLine()"
-- vim.opt.winbar = '%!v:lua.StatusLine()'
vim.opt.laststatus = 3
vim.keymap.set("n", "Y", "^v$y", { desc = "Copy whole line" })
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
vim.keymap.set("i", "<C-c>", "<esc>")
-- [[ Is there a better way to exit out of insert ? I don't think so.
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
-- ]]
-- [[ When jumping around I want cursor to always remain at the center of the screen.
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-o>", "<C-o>zz")
vim.keymap.set("n", "<C-i>", "<C-i>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
-- ]]
vim.keymap.set("n", "Q", "<cmd>q<CR>")
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("n", "<M-Left>", "<c-w>5>")
vim.keymap.set("n", "<M-Right>", "<c-w>5<")
vim.keymap.set("n", "<M-Up>", "<C-W>+")
vim.keymap.set("n", "<M-Down>", "<C-W>-")
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set("t", "<C-w><C-w>", "<cmd>wincmd w<cr>")
vim.keymap.set({ "n", "t" }, "<C-j>", require("nvim-terminal")("bottom"))
vim.keymap.set("n", "<leader>i", ":edit $MYVIMRC<CR>")
vim.keymap.set("n", "<C-q>", function()
    local wins = vim.api.nvim_list_wins()
    local has_qf_open = false
    for _, win in ipairs(wins) do
        local buf = vim.api.nvim_win_get_buf(win)
        if vim.api.nvim_get_option_value('buftype', { buf = buf }) == 'quickfix' then
            has_qf_open = true
        end
    end
    if has_qf_open then
        vim.cmd([[ cclose ]])
    else
        vim.cmd([[ copen ]])
    end
end, { desc = "Open Quickfix list" })
vim.keymap.set("n", "{", "<cmd>cprev<CR>")
vim.keymap.set("n", "}", "<cmd>cnext<CR>")

vim.api.nvim_create_autocmd("TextYankPost", { -- Highlight yanked text
    group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
    callback = function()
        vim.highlight.on_yank()
    end,
})
