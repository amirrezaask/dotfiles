local paq_install_path = vim.fn.stdpath("data") .. "/site/pack/paqs/start/paq-nvim"

if vim.fn.empty(vim.fn.glob(paq_install_path)) > 0 then -- Installing nvim-paq package manager if not installed
    print("Installing paq-nvim...")
    vim.fn.system({ "git", "clone", "--depth=1", "https://github.com/savq/paq-nvim.git", paq_install_path })
    print("paq-nvim installed! Restart Neovim and run :PaqInstall")
end


require("paq")({
    "stevearc/conform.nvim",
    "neovim/nvim-lspconfig",
    "williamboman/mason.nvim",
    "saghen/blink.cmp",
    "nvim-treesitter/nvim-treesitter",
    "folke/ts-comments.nvim",
    "nvim-tree/nvim-web-devicons",
    "folke/tokyonight.nvim",
    "tpope/vim-fugitive",
    { "rose-pine/neovim", as = "rose-pine" },
    { "catppuccin/nvim",  as = "catppuccin" },
    "amirrezaask/nvim-terminal.lua",
    "amirrezaask/nvim-blue.lua",
    "amirrezaask/nvim-sitruuna.lua",
})

vim.opt.runtimepath:append(vim.fn.expand("~/src/nvim-finder.lua"))

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

vim.cmd.colorscheme("nvim-blue")

-- local base16 = require("base16")
--
-- local norcalli = base16.theme_from_array {
--     "121b2b",
--     "213554",
--     "1d3872",
--     "80b2d6",
--     "3aa3e9",
--     "abb2bf",
--     "b6bdca",
--     "c8ccd4",
--     "f04c75",
--     "d19a66",
--     "e5c07b",
--     "98c379",
--     "56b6c2",
--     "01bfef",
--     "c678dd",
--     "be5046",
-- }
--
-- base16(norcalli, true)

vim.g.mapleader = " "      -- <leader> key for keymaps mapped to <Space>
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
vim.opt.showmode = false
vim.opt.sw = 4 -- TABs and indentation
vim.opt.ts = 4
vim.opt.expandtab = true
vim.g.netrw_browse_split = 0 -- minimal netrw (vim default file manager)
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.opt.guicursor = ""
vim.opt.timeoutlen = 300 -- vim update time
vim.opt.updatetime = 250
vim.opt.cursorline = false
vim.opt.number = true             -- Line numbers
vim.opt.mouse = "a"               -- Enable mouse in all modes.
vim.opt.clipboard = "unnamedplus" -- Clipboard
vim.opt.ignorecase = true         -- Search has case insensitive by default, but if pattern has some upper case letters, it will be case sensitive.
vim.opt.smartcase = true          -- Search has case insensitive by default, but if pattern has some upper case letters, it will be case sensitive.
vim.opt.completeopt = { "fuzzy", "menu", "noinsert", "noselect", "popup" }
vim.opt.inccommand = ""           -- Preview all substitutions(replacements).
vim.opt.scrolloff = 10            -- Minimal number of screen lines to keep above and below the cursor.
-- vim.opt.laststatus = 3            -- Global statusline
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

    return "%l:%c %m%r%h%w%F%=" .. mode .. " %y"
end

vim.opt.statusline = "%!v:lua.StatusLine()"
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

require("blink.cmp").setup {
    keymap = { preset = "enter" },
    fuzzy = {
        prebuilt_binaries = {
            force_version = "v1.1.1",
        },
    },
}

-- [[ Toggle Quick fix list
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
-- ]]

vim.api.nvim_create_autocmd("TextYankPost", { -- Highlight yanked text
    group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
    callback = function()
        vim.highlight.on_yank()
    end,
})
F = require("nvim-finder")

vim.keymap.set("n", "<leader><leader>", F.files, {})
vim.keymap.set("n", "<leader>ff", F.files, {})
vim.keymap.set("n", "<C-p>", F.git_files, {})
vim.keymap.set("n", "<leader>fg", F.git_files, {})
vim.keymap.set("n", "<leader>fd", function() F.files { path = "~/.dotfiles" } end, {})
vim.keymap.set("n", "??", F.ripgrep_fuzzy, {})
vim.keymap.set("n", "<leader>fb", F.buffers, {})
vim.keymap.set("n", "<leader>h", F.helptags, {})
vim.keymap.set("n", "<leader>d", F.diagnostics_buffer, {})
vim.keymap.set("n", "<leader>D", F.diagnostics, {})
vim.keymap.set("n", "<leader>o", F.lsp_document_symbols, {})
vim.keymap.set("n", "<leader>O", F.lsp_workspace_symbols, {})

-- treesitter
require("nvim-treesitter.configs").setup({
    auto_install = false,
    sync_install = false,
    ensure_installed = { "lua", "go", "gomod", "markdown", "php", "c", "cpp" },
    ignore_install = {},
    highlight = { enable = true },
    modules = {},
})

require("ts-comments").setup()

-- Mason
require("mason").setup({ ensure_installed = { "gopls" } })
local process_path = os.getenv("PATH")
vim.fn.setenv("PATH", process_path .. ":" .. vim.fn.stdpath("data") .. "/mason/bin")

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

require("conform").setup({
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
        local has_fzf, fzf = pcall(require, "fzf-lua")
        if has_fzf then
            references = fzf.lsp_references
            implementations = fzf.lsp_implementations
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
