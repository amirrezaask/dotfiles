vim.g.mapleader = " "
vim.o.wrap = true
vim.o.breakindent = true
vim.o.signcolumn = 'yes'
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
vim.o.number = true
vim.o.clipboard = "unnamedplus"
vim.o.ignorecase = true; vim.o.smartcase = true
vim.o.completeopt = "fuzzy,menu,noinsert,noselect,popup"
vim.o.statusline = "[%l:%c]%=%m%r%q%h%f%=%y"
vim.o.winborder = 'rounded'
local map = vim.keymap.set
map("n", "Y", "^v$y", { desc = "Copy whole line" })
map("t", "<esc>", [[<C-\><C-n>]])
map("i", "<C-c>", "<esc>")
map("n", "<C-d>", "<C-d>zz")
map("n", "<C-u>", "<C-u>zz")
map("n", "n", "nzz")
map("n", "N", "Nzz")
map("i", "jk", "<ESC>")
map("i", "kj", "<ESC>")
map("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
map("n", "j", "gj"); vim.keymap.set("n", "k", "gk")
map("t", "<C-w><C-w>", "<cmd>wincmd w<cr>")
map("n", "<leader>i", ":edit $MYVIMRC<CR>")
map("n", "<C-q>", function()
    local wins = vim.api.nvim_list_wins()
    local has_qf_open = false
    for _, win in ipairs(wins) do
        local buf = vim.api.nvim_win_get_buf(win)
        if vim.api.nvim_get_option_value("buftype", { buf = buf }) == "quickfix" then
            has_qf_open = true
        end
    end
    if has_qf_open then
        vim.cmd([[ cclose ]])
    else
        vim.cmd([[ copen ]])
    end
end, { desc = "Toggle Quickfix list" })
vim.keymap.set("n", "{", "<cmd>cprev<CR>")
vim.keymap.set("n", "}", "<cmd>cnext<CR>")

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

require("lazy").setup {
    { "folke/snacks.nvim", dependencies = { 'nvim-tree/nvim-web-devicons' }, opts = { picker = { enabled = true, sources = { files = { layout = { preview = false }}, git_files = { layout = { preview = false } } } } } },
    { "folke/tokyonight.nvim" },
    { "rose-pine/neovim", name = "rose-pine" },
    { "neovim/nvim-lspconfig" },
    { "williamboman/mason.nvim", opts = { ensure_installed = { "gopls" } } },
    { "saghen/blink.cmp", version = "1.*", opts = { keymap = { preset = "enter" }, cmdline = { enabled = false } } },
    { "nvim-treesitter/nvim-treesitter", config = function() require("nvim-treesitter.configs").setup{ ensure_installed = { "lua", "go", "gomod", "php" }, highlight = { enable = true } } end },
}

vim.cmd.colorscheme(vim.env.NVIM_COLORSCHEME or "tokyonight-night")

for _, lsp in ipairs({ "gopls", "intelephense", "rust_analyzer", "zls" }) do require("lspconfig")[lsp].setup {} end
require("lspconfig").lua_ls.setup({ settings = { Lua = { diagnostics = { globals = { "vim" } } } } })

vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
        map("n", "[[", function() vim.diagnostic.jump({ count = -1 }) end, { buffer = args.buf })
        map("n", "]]", function() vim.diagnostic.jump({ count = 1 }) end, { buffer = args.buf })
        map("n", "C-]", vim.lsp.buf.definition, { buffer = args.buf })
        map("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
        map("n", "gD", vim.lsp.buf.declaration, { buffer = args.buf })
        map("n", "gi", vim.lsp.buf.references, { buffer = args.buf })
        map("n", "gr", vim.lsp.buf.implementation, { buffer = args.buf })
        map("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
        map("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
        map("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
        map({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, { buffer = args.buf })
        map("n", "<leader>l", vim.diagnostic.open_float, { buffer = args.buf })
        map("n", "<leader>q", vim.diagnostic.setloclist, { buffer = args.buf })
    end,
})

Snacks = require("snacks")
P = Snacks.picker
map("n", "<leader><leader>", P.files, {})
map("n", "<C-p>", P.git_files, {})
map("n", "<leader>fd", function() P.files { cwd = "~/.dotfiles" } end, {})
map("n", "??", P.grep, {})
map("v", "??", P.grep_word, {})
map("n", "<leader>h", P.help, {})
map("n", "<leader>d", P.diagnostics_buffer, {})
map("n", "<leader>D", P.diagnostics, {})
map("n", "<leader>o", P.lsp_symbols, {})
map("n", "<leader>O", P.lsp_workspace_symbols, {})
