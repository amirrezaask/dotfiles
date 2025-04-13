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
vim.o.number = true
vim.o.clipboard = "unnamedplus"
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.statusline = "%l:%c%=%m%r%q%h%f%=%y"
local B = vim.keymap.set
B("n", "Y", "^v$y", { desc = "Copy whole line" })
B("t", "<esc>", [[<C-\><C-n>]])
B("i", "<C-c>", "<esc>")
B("n", "<C-d>", "<C-d>zz")
B("n", "<C-u>", "<C-u>zz")
B("n", "n", "nzz")
B("n", "N", "Nzz")
B("i", "jk", "<ESC>")
B("i", "kj", "<ESC>")
B("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
B("n", "j", "gj")
B("n", "k", "gk")
B("n", "<leader>i", ":edit $MYVIMRC<CR>")
B("n", "{", "<cmd>cprev<CR>")
B("n", "}", "<cmd>cnext<CR>")
B("n", "<C-q>", function()
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

local function configure_lsp(name, opts)
    vim.lsp.config[name] = opts
    vim.lsp.enable(name)
end
configure_lsp("lua_ls", {
    cmd = { "lua-language-server" },
    filetypes = { "lua" },
    root_markers = {
        ".luarc.json",
        ".luarc.jsonc",
        ".luacheckrc",
        ".stylua.toml",
        "stylua.toml",
        "selene.toml",
        "selene.yml",
        ".git",
    },
    settings = { Lua = { diagnostics = { globals = { "vim" } } } },
})

configure_lsp("gopls", { cmd = { "gopls" }, filetypes = { "go" }, root_markers = { "go.mod", ".git" } })
configure_lsp("intelephense",
    { cmd = { "intelephense" }, filetypes = { "php" }, root_markers = { "composer.json", ".git" } })

vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
        B("n", "[[", function()
            vim.diagnostic.jump({ count = -1 })
        end, { buffer = args.buf })
        B("n", "]]", function()
            vim.diagnostic.jump({ count = 1 })
        end, { buffer = args.buf })
        B("n", "C-]", vim.lsp.buf.definition, { buffer = args.buf })
        B("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
        B("n", "gD", vim.lsp.buf.declaration, { buffer = args.buf })
        B("n", "gr", vim.lsp.buf.references, { buffer = args.buf })
        B("n", "gi", vim.lsp.buf.implementation, { buffer = args.buf })
        B("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
        B("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
        B("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
        B({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, { buffer = args.buf })
        B("n", "<leader>l", vim.diagnostic.open_float, { buffer = args.buf })
        B("n", "<leader>q", vim.diagnostic.setloclist, { buffer = args.buf })
        vim.api.nvim_create_autocmd("BufWritePre", {
            buffer = args.buf,
            callback = function(args)
                local clients = vim.lsp.get_clients({ bufnr = args.buf })
                local params = vim.lsp.util.make_range_params()
                params.context = { only = { "source.organizeImports" } }
                for _, client in pairs(clients) do
                    if client:supports_method("textDocument/codeAction") then
                        local result = vim.lsp.buf_request_sync(args.buf, "textDocument/codeAction", params, 1000)
                        for _, res in ipairs(result or {}) do
                            for _, action in pairs(res.result or {}) do
                                if action.edit or type(action.command) == "table" then
                                    if action.edit then
                                        vim.lsp.util.apply_workspace_edit(action.edit, client.offset_encoding)
                                    end
                                    if type(action.command) == "table" then
                                        client:execute_cmd(action.command)
                                    end
                                end
                            end
                        end
                        vim.lsp.buf.format({ bufnr = args.buf })
                    end
                end
            end,
        })
    end,
})

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
    { "folke/tokyonight.nvim" }, { "rose-pine/neovim", name = 'rose-pine' }, { "catppuccin/nvim", name = 'catppuccin' },
    { "folke/snacks.nvim", dependencies = { "nvim-tree/nvim-web-devicons" }, config = function()
        Snacks = require("snacks")
        Snacks.setup { picker = { enabled = true } }
        P = Snacks.picker
        function FallbackToSnacks()
            B("n", "<leader><leader>", P.files, {})
            B("n", "<C-p>", P.git_files, {})
            B("n", "<leader>fd", function() P.files { cwd = "~/.dotfiles" } end, {})
            B("n", "??", P.grep, {})
            B("v", "??", P.grep_word, {})
            B("n", "<leader>h", P.help, {})
            B("n", "<leader>d", P.diagnostics_buffer, {})
            B("n", "<leader>D", P.diagnostics, {})
            B("n", "<leader>o", P.lsp_symbols, {})
            B("n", "<leader>O", P.lsp_workspace_symbols, {})
            B({ "n", "t" }, "<C-j>", Snacks.terminal.toggle, {})
        end

        FallbackToSnacks()
    end,
    },
    { "williamboman/mason.nvim", opts = {} },
    { "saghen/blink.cmp",        version = "1.*", opts = { keymap = { preset = "enter" }, cmdline = { enabled = false } } },
    { "nvim-treesitter/nvim-treesitter", config = function()
        require("nvim-treesitter.configs").setup {
            ensure_installed = { "lua", "go", "gomod", "php" }, highlight = { enable = true }, }
    end
    },
}
vim.cmd.colorscheme(vim.env.NVIM_COLORSCHEME or "tokyonight-night")
