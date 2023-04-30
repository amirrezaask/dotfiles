-- ==========================================================================
-- ============================ Vim Options ====================================
-- ==========================================================================
vim.opt.number = true -- Line numbers
vim.opt.relativenumber = true -- Relative line numbers
vim.opt.errorbells = false
vim.opt.smartindent = true
vim.opt.wrap = false
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = true
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.termguicolors = true
vim.opt.signcolumn = "yes"
vim.opt.isfname:append "@-@"
vim.opt.updatetime = 50
vim.opt.guicursor = "" -- Don't style cursor in different modes, just a box would suffice
vim.opt.shortmess:append "c" -- Don't pass messages to |ins-completion-menu|.
vim.opt.shortmess:append "I" -- No Intro message
vim.opt.clipboard:append "unnamedplus" -- use system clipboard as default register.
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.cursorline = false
vim.opt.sw = 4
vim.opt.ts = 4
vim.opt.expandtab = true
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.opt.laststatus = 2
vim.opt.timeoutlen = 300

-- ==========================================================================
-- ========================= Plugins ========================================
-- ==========================================================================
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
vim.opt.rtp:prepend(lazypath)

require("lazy").setup {
    "norcalli/nvim-colorizer.lua", -- Colorize colorcodes in neovim using blazingly fast LUA code
    "ellisonleao/gruvbox.nvim", -- Best theme of all time ?
    "amirrezaask/themes", -- My own custom created themes
    "folke/tokyonight.nvim", -- folkkkkkeeeeee
    { "rose-pine/neovim", name = "rose-pine" },
    "numToStr/Comment.nvim", -- Comment
    { -- telescope: Fuzzy finding and searching interface
        "nvim-telescope/telescope.nvim",
        dependencies = { "nvim-lua/plenary.nvim", { "nvim-telescope/telescope-fzf-native.nvim", build = "make" } },
    },
    { -- Treesitter syntax highlighting and text objects.
        "nvim-treesitter/nvim-treesitter",
        dependencies = { "nvim-treesitter/nvim-treesitter-textobjects", "nvim-treesitter/playground" },
    },
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            { "williamboman/mason.nvim", dependencies = { "williamboman/mason-lspconfig.nvim" } },
            "jose-elias-alvarez/null-ls.nvim",
            {
                "hrsh7th/nvim-cmp", -- Autocompletion popup
                dependencies = {
                    "hrsh7th/cmp-nvim-lsp",
                    "hrsh7th/cmp-vsnip",
                    "hrsh7th/vim-vsnip",
                    "hrsh7th/cmp-path",
                    "hrsh7th/cmp-buffer",
                },
            },
        },
    },
    "stevearc/oil.nvim", -- File manager like a BOSS
    "pbrisbin/vim-mkdir", -- Automatically create directory if not exists
    "fladson/vim-kitty", -- Support Kitty terminal config syntax
    "towolf/vim-helm", -- Support for helm template syntax
    "tpope/vim-surround", -- surrounding text objects
    "kevinhwang91/nvim-bqf", -- Preview quickfix list item.
    "tpope/vim-eunuch", -- Helper commands like :Rename, :Move, :Delete, :Remove, ...
    "tpope/vim-sleuth", -- Heuristically set buffer options
    "windwp/nvim-autopairs", -- Auto insert pairs like () [] {}
    "lewis6991/gitsigns.nvim", -- Signs next to line numbers to show git status of a line
    "tpope/vim-fugitive", -- Second best Git client ( first one is emacs magit )
    "dag/vim-fish", -- Vim fish syntax
    "jansedivy/jai.vim", -- Jai from Jonathan Blow
    "akinsho/toggleterm.nvim", -- Terminal inside neovim
    "folke/which-key.nvim", -- Cheat your way through keybindings
    "nvim-tree/nvim-tree.lua", -- Tree file explorer
}

-- ==========================================================================
-- ========================= Plugins configuration ==========================
-- ==========================================================================
require("Comment").setup() -- Comment code with ease
require("which-key").setup()
require("nvim-tree").setup()
require("telescope").setup {} -- Best fuzzy finder
require("telescope").load_extension "fzf" -- load fzf awesomnes into Telescope
require("nvim-treesitter.configs").setup { -- Setup treesitter text objects module + highlight
    ensure_installed = { "json", "yaml", "c", "cpp", "lua", "rust", "go", "python", "php" },
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
-- Autocompletion menu using nvim-cmp
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)
local cmp = require "cmp"
cmp.setup {
    window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
    },
    snippet = {
        expand = function(args) vim.fn["vsnip#anonymous"](args.body) end,
    },
    mapping = cmp.mapping.preset.insert {
        ["<CR>"] = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
    },
    sources = {
        { name = "nvim_lsp" },
        { name = "buffer" },
        { name = "path" },
    },
}
require("mason").setup {}
for _, pkg in ipairs { "stylua", "golangci-lint", "goimports", "yamlfmt" } do -- ensure these tools are installed
    if not require("mason-registry").is_installed(pkg) then require("mason.api.command").MasonInstall { pkg } end
end

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
local mason_lspconfig = require "mason-lspconfig"
mason_lspconfig.setup {
    ensure_installed = vim.tbl_keys(lsp_servers),
}

for server, config in pairs(lsp_servers) do
    require("lspconfig")[server].setup(config)
end

-- Hover and signature help windows have rounded borders
vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" })
vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" })

-- LspInfo window have rounded border
require("lspconfig.ui.windows").default_options.border = "rounded"

vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = { "*.rs", "*.lua" },
    callback = function(_) vim.lsp.buf.format() end,
})

-- Null-LS helps us hook non LSP tools like linters into Neovim LSP infrastructure.
require("null-ls").setup {
    sources = {
        require("null-ls").builtins.code_actions.gitsigns,
        require("null-ls").builtins.diagnostics.golangci_lint,
        require("null-ls").builtins.diagnostics.trail_space.with { disabled_filetypes = { "NvimTree" } },
        require("null-ls").builtins.formatting.stylua,
        require("null-ls").builtins.formatting.goimports,
    },
}
-- gitsigns setup
require("gitsigns").setup {
    signs = {
        add = { text = "+" },
        change = { text = "~" },
        delete = { text = "_" },
        topdelete = { text = "‾" },
        changedelete = { text = "~" },
    },
}
-- Fugitive helper command to help ease git push :)
vim.api.nvim_create_user_command("Gp", function(_, _) vim.cmd.Git "push" end, {})

-- Toggleterm setup
require("toggleterm").setup {
    size = 20,
    direction = "horizontal",
}

-- ==========================================================================
-- ========================= Colorscheme ====================================
-- ==========================================================================
require("gruvbox").setup {
    contrast = "hard",
    italic = {
        strings = false,
        comments = false,
        operators = false,
        folds = false,
    },
}
require("rose-pine").setup {
    disable_background = true,
}
pcall(vim.cmd.colorscheme, "rose-pine")
-- ==========================================================================
-- ========================= Keybindings ====================================
-- ==========================================================================
vim.g.mapleader = " "
local bind = vim.keymap.set
-- Editing
bind("t", "<Esc>", "<C-\\><C-n>")
bind("t", "jk", "<C-\\><C-n>")
bind("t", "kj", "<C-\\><C-n>")
bind("i", "jk", "<esc>")
bind("i", "kj", "<esc>")
bind("n", "Y", "y$")
-- Window management
bind("n", "<leader>v", "<cmd>vsplit<CR>", { desc = "Split vertically" })
bind("n", "<leader>h", "<cmd>split<CR>", { desc = "Split horizontaly" })
-- Git
bind("n", "<leader>gs", vim.cmd.Git, { desc = "Git status" })
bind("n", "<leader>b", function() require("gitsigns").blame_line { full = true } end, { desc = "Git blame line" })
bind("n", "<leader>d", function() require("gitsigns").diffthis "~" end, { desc = "Diff current file with HEAD" })
-- Navigation
local telescope_current_theme = { previewer = false }
local telescope_builtin = require "telescope.builtin"
bind("n", "<C-d>", "<C-d>zz")
bind("n", "<C-u>", "<C-u>zz")
bind("n", "<C-p>", function() telescope_builtin.git_files(telescope_current_theme) end, { desc = "Telescope Git Files" })
bind("n", "<leader>pf", function() telescope_builtin.find_files(telescope_current_theme) end,
    { desc = "Telescope Find files" })
bind("n", "<leader><leader>", function() telescope_builtin.find_files(telescope_current_theme) end,
    { desc = "Telescope Find files" })
bind("n", "<C-f>", function() telescope_builtin.current_buffer_fuzzy_find(telescope_current_theme) end,
    { desc = "Current File Search" })
bind("n", "<leader>o", function() telescope_builtin.treesitter(telescope_current_theme) end,
    { desc = "Search Symbols In Current File" })
bind("n", "??", function() telescope_builtin.live_grep() end, { desc = "Live Grep" })
bind("n", "Q", "<NOP>")
bind("n", "{", ":cprev<CR>")
bind("n", "}", ":cnext<CR>")
bind("v", "J", ":m '>+1<CR>gv=gv")
bind("v", "K", ":m '<-2<CR>gv=gv")
bind("n", "n", "nzz")
bind("n", "N", "Nzz")
bind("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
-- LSP
vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
        local bufnr = args.buf
        vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
        local buffer = function(desc) return { buffer = bufnr, desc = desc } end
        bind("n", "gd", vim.lsp.buf.definition, buffer "Goto Definition")
        bind("n", "gD", vim.lsp.buf.declaration, buffer "Goto Declaration")
        bind("n", "gi", vim.lsp.buf.implementation, buffer "Goto Implementation")
        bind("n", "gr", vim.lsp.buf.references, buffer "Goto References")
        bind("n", "R", vim.lsp.buf.rename, buffer "Rename")
        bind("n", "K", vim.lsp.buf.hover, buffer "Hover")
        bind("n", "gf", vim.lsp.buf.format, buffer "Format Document")
        bind("n", "gl", vim.diagnostic.open_float, buffer "")
        bind("n", "gp", vim.diagnostic.goto_prev, buffer "Next Diagnostic")
        bind("n", "gn", vim.diagnostic.goto_next, buffer "Previous Diagnostic")
        bind("n", "C", vim.lsp.buf.code_action, buffer "Code Actions")
        bind("n", "<C-s>", vim.lsp.buf.signature_help, buffer "Signature Help")
        bind("i", "<C-s>", vim.lsp.buf.signature_help, buffer "Signature Help")
        bind("n", "<leader>o", function() require("telescope.builtin").lsp_document_symbols(telescope_current_theme) end,
            buffer "Document Symbols")
    end,
})
bind({ "n", "t", "i" }, "<C-j>", vim.cmd.ToggleTerm, { desc = "ToggleTerm" }) -- Terminal
bind({ "n" }, "<C-b>", vim.cmd.NvimTreeToggle, { desc = "NvimTreeToggle" }) -- Tree File Explorer
