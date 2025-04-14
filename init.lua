local paq_install_path = vim.fn.stdpath("data") .. "/site/pack/paqs/start/paq-nvim"

if vim.fn.empty(vim.fn.glob(paq_install_path)) > 0 then -- Installing nvim-paq package manager if not installed
    print("Installing paq-nvim...")
    vim.fn.system({ "git", "clone", "--depth=1", "https://github.com/savq/paq-nvim.git", paq_install_path })
    print("paq-nvim installed! Restart Neovim and run :PaqInstall")
end

require("paq")({
    "folke/tokyonight.nvim",
    'amirrezaask/nvim-blue.lua',
    "ellisonleao/gruvbox.nvim",
    { "rose-pine/neovim", as = "rose-pine" },
    "ibhagwan/fzf-lua",
    'amirrezaask/nvim-terminal.lua',
    "williamboman/mason.nvim",
    "nvim-treesitter/nvim-treesitter",
    { "saghen/blink.cmp", branch = "v1.1.1" },
})

require("gruvbox").setup({ contrast = "hard" })

vim.cmd.colorscheme(vim.env.NVIM_COLORSCHEME or "rose-pine-moon")
function Transparent()
    vim.cmd [[
        hi Normal guibg=none
        hi NormalNC guibg=none
        hi FloatBorder guibg=none
        hi LineNr guibg=none
        hi SignColumn guibg=none
        hi NormalFloat guibg=none
    ]]
end

Transparent()
vim.g.mapleader = " "
vim.o.wrap = true
vim.o.breakindent = true
vim.o.signcolumn = "yes"
vim.o.swapfile = false
vim.o.backup = false
vim.o.undofile = true
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.showmode = false
vim.o.shiftwidth = 4
vim.o.tabstop = 4
vim.o.expandtab = true
vim.o.timeoutlen = 300
vim.o.updatetime = 250
vim.o.clipboard = "unnamedplus"
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.statusline = "%l:%c%=%m%r%q%h%f%=%y"
vim.o.signcolumn = 'no'
local keymap = vim.keymap.set
keymap("n", "Y", "^v$y", { desc = "Copy whole line" })
keymap("t", "<esc>", [[<C-\><C-n>]])
keymap("i", "<C-c>", "<esc>")
keymap("n", "<C-d>", "<C-d>zz")
keymap("n", "<C-u>", "<C-u>zz")
keymap("n", "n", "nzz")
keymap("n", "N", "Nzz")
keymap("i", "jk", "<ESC>")
keymap("i", "kj", "<ESC>")
keymap("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
keymap("n", "j", "gj")
keymap("n", "k", "gk")
keymap("n", "<leader>i", ":edit $MYVIMRC<CR>")
keymap("n", "{", "<cmd>cprev<CR>")
keymap("n", "}", "<cmd>cnext<CR>")
keymap("n", "<C-q>", function()
    local wins = vim.api.nvim_list_wins()
    for _, win in ipairs(wins) do
        local buf = vim.api.nvim_win_get_buf(win)
        if vim.api.nvim_get_option_value("buftype", { buf = buf }) == "quickfix" then
            vim.cmd.cclose()
            return
        end
    end
    vim.cmd.copen()
end, { desc = "Toggle Quickfix list" })

local function configure_lsp(name, opts)
    vim.lsp.config[name] = opts
    vim.lsp.enable(name)
end

configure_lsp("lua_ls", {
    cmd = { "lua-language-server" },
    filetypes = { "lua" },
    root_markers = { ".git" },
    settings = { Lua = { diagnostics = { globals = { "vim" } } } },
})

configure_lsp("gopls", { cmd = { "gopls" }, filetypes = { "go" }, root_markers = { "go.mod", ".git" } })
configure_lsp("intelephense",
    { cmd = { "intelephense", '--stdio' }, filetypes = { "php" }, root_markers = { "composer.json", ".git" } })

vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
        local has_fzf, _ = pcall(require, "fzf-lua")
        keymap("n", "[[", function()
            vim.diagnostic.jump({ count = -1 })
        end, { buffer = args.buf })
        keymap("n", "]]", function()
            vim.diagnostic.jump({ count = 1 })
        end, { buffer = args.buf })
        keymap("n", "C-]", has_fzf and require("fzf-lua").lsp_definition or vim.lsp.buf.definition,
            { buffer = args.buf })
        keymap("n", "gd", has_fzf and require("fzf-lua").lsp_definition or vim.lsp.buf.definition,
            { buffer = args.buf })
        keymap("n", "gD", has_fzf and require("fzf-lua").lsp_declaration or vim.lsp.buf.declaration,
            { buffer = args.buf })
        keymap("n", "gr", has_fzf and require("fzf-lua").lsp_references or vim.lsp.buf.references,
            { buffer = args.buf })
        keymap("n", "gi", has_fzf and require("fzf-lua").lsp_implementation or vim.lsp.buf.implementation,
            { buffer = args.buf })
        keymap("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
        keymap("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
        keymap("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
        keymap({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, { buffer = args.buf })
        keymap("n", "<leader>l", vim.diagnostic.open_float, { buffer = args.buf })
        keymap("n", "<leader>q", vim.diagnostic.setloclist, { buffer = args.buf })
        vim.api.nvim_create_autocmd("BufWritePre", {
            buffer = args.buf,
            callback = function(args)
                local old_print = print
                print = function(...) end
                vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } }, apply = true })
                print = old_print
                vim.lsp.buf.format({ bufnr = args.buf })
                vim.cmd.write()
            end,
        })
    end,
})


keymap({ "n", "t" }, "<C-j>", require("nvim-terminal")("bottom"))

Fzf = require("fzf-lua")
Fzf.setup {
    files = {
        previewer = false,
    }
}

keymap("n", "<leader><leader>", Fzf.files)
keymap("n", "<leader>b", Fzf.buffers)
keymap("n", "<leader>h", Fzf.helptags)
keymap("n", "<C-p>", Fzf.git_files)
keymap("n", "??", Fzf.grep)
keymap("v", "??", Fzf.grep_cword)
keymap("n", "<leader>o", Fzf.lsp_document_symbols)
keymap("n", "<leader>O", Fzf.lsp_workspace_symbols)
keymap("n", "<leader>fd", function() Fzf.files({ cwd = "~/.dotfiles" }) end)

require("mason").setup()

require("blink.cmp").setup { keymap = { preset = "enter" }, cmdline = { enabled = false } }

require("nvim-treesitter.configs").setup { ensure_installed = { "lua", "go", "gomod", "php" }, highlight = { enable = true }, }
