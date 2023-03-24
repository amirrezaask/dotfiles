vim.opt.number = true -- Line numbers
vim.opt.relativenumber = true -- Relative line numbers
vim.opt.errorbells = false
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.wrap = false
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv "HOME" .. "/.vim/undodir"
vim.opt.undofile = true
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.termguicolors = true
vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.isfname:append "@-@"
vim.opt.updatetime = 50
vim.opt.guicursor = ""
vim.opt.shortmess:append "c" -- Don't pass messages to |ins-completion-menu|.
vim.opt.shortmess:append "I" -- No Intro message
vim.opt.clipboard:append "unnamedplus" -- use system clipboard as default register.
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.cursorline = true
vim.g.transparent = false

-- Netrw
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25

vim.g.mapleader = " "

vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

vim.keymap.set("n", "<leader>vs", "<cmd>vsplit<CR>")
vim.keymap.set("n", "<leader>sp", "<cmd>split<CR>")

vim.keymap.set("n", "Q", "<NOP>")

vim.keymap.set("n", "{", ":cprev<CR>")
vim.keymap.set("n", "}", ":cnext<CR>")

vim.keymap.set("n", "Y", "y$")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")

vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")

vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })

vim.keymap.set("t", "<Esc>", "<C-\\><C-n>")
vim.keymap.set("t", "jk", "<C-\\><C-n>")
vim.keymap.set("t", "kj", "<C-\\><C-n>")

vim.keymap.set("i", "jk", "<esc>")
vim.keymap.set("i", "kj", "<esc>")

-- Easier split navigation
vim.keymap.set("n", "<C-h>", "<C-w>h")
vim.keymap.set("n", "<C-j>", "<C-w>j")
vim.keymap.set("n", "<C-k>", "<C-w>k")
vim.keymap.set("n", "<C-l>", "<C-w>l")

vim.keymap.set("t", "<C-h>", "<cmd>wincmd h<CR>")
vim.keymap.set("t", "<C-j>", "<cmd>wincmd j<CR>")
vim.keymap.set("t", "<C-k>", "<cmd>wincmd k<CR>")
vim.keymap.set("t", "<C-l>", "<cmd>wincmd l<CR>")

-- Easier split resizing
vim.keymap.set("n", "<A-l>", "<C-w><")
vim.keymap.set("n", "<A-h>", "<C-w>>")
vim.keymap.set("n", "<A-j>", "<C-w>-")
vim.keymap.set("n", "<A-k>", "<C-w>+")

vim.keymap.set("t", "<A-l>", "<C-\\><C-n><C-w><")
vim.keymap.set("t", "<A-h>", "<C-\\><C-n><C-w>>")
vim.keymap.set("t", "<A-j>", "<C-\\><C-n><C-w>-")
vim.keymap.set("t", "<A-k>", "<C-\\><C-n><C-w>+")

local function setup(mod, opts) require(mod).setup(opts) end

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
  { "folke/tokyonight.nvim" },
  { "rose-pine/neovim", name = "rose-pine" },
  { "catppuccin/nvim", name = "catppuccin" },
  { "Mofiqul/dracula.nvim" },
  { "ellisonleao/gruvbox.nvim" },
  { "eemed/sitruuna.vim" },
  { "numToStr/Comment.nvim" },
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim", { "nvim-telescope/telescope-fzf-native.nvim", build = "make" } },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects", "nvim-treesitter/playground" },
  },
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "folke/neodev.nvim",
      "j-hui/fidget.nvim",
    },
  },
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-vsnip",
      "hrsh7th/vim-vsnip",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-buffer",
    },
  },
  { "jose-elias-alvarez/null-ls.nvim" },
  { "stevearc/oil.nvim" }, -- File manager like a BOSS
  { "pbrisbin/vim-mkdir" }, -- Automatically create directory if not exists
  { "fladson/vim-kitty" }, -- Support Kitty terminal config syntax
  { "towolf/vim-helm" }, -- Support for helm template syntax
  { "tpope/vim-surround" }, -- surrounding text objects
  { "kevinhwang91/nvim-bqf" }, -- Preview quickfix list item.
  { "tpope/vim-eunuch" }, -- Helper commands like :Rename, :Move, :Delete, :Remove, ...
  { "tpope/vim-sleuth" }, -- Heuristically set buffer options
  { "windwp/nvim-autopairs" }, -- Auto insert pairs like () [] {}
  { "lewis6991/gitsigns.nvim" }, -- Signs next to line numbers to show git status of a line
  { "tpope/vim-fugitive" }, -- Best Git Client after magit :)
  { "fatih/vim-go" }, -- Golang tools and code actions
  { "akinsho/toggleterm.nvim" }, -- Terminal emulator that we deserve
  { "dag/vim-fish" }, -- Vim fish syntax
  { "imsnif/kdl.vim" },
  { "jansedivy/jai.vim" },
  { "aserowy/tmux.nvim" },
}

-- Setup Colorschemes
setup("tokyonight", { transparent = vim.g.transparent })
setup("rose-pine", { disable_background = vim.g.transparent, disable_float_background = vim.g.transparent })
setup("catppuccin", { transparent_background = vim.g.transparent })
setup("dracula", { transparent_bg = vim.g.transparent })
setup("gruvbox", { transparent_mode = vim.g.transparent })
pcall(vim.cmd.colorscheme, "catppuccin")

setup("Comment", {})
setup("oil", {})

-- nvim-cmp
local cmp = require "cmp"
cmp.setup {
  snippet = {
    expand = function(args) vim.fn["vsnip#anonymous"](args.body) end,
  },
  mapping = cmp.mapping.preset.insert {
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete {},
    ["<CR>"] = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
    ["<Tab>"] = nil,
    ["<S-Tab>"] = nil,
  },
  sources = {
    { name = "nvim_lsp" },
    { name = "buffer" },
    { name = "path" },
  },
}

-- Mason
require("mason").setup {}
for _, pkg in ipairs { "stylua", "golangci-lint", "goimports", "gofumpt", "yamlfmt" } do -- ensure these tools are installed
  if not require("mason-registry").is_installed(pkg) then require("mason.api.command").MasonInstall { pkg } end
end

-- Mason lsp
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

local mason_lspconfig = require "mason-lspconfig"
local ensure_installed = {
  gopls = {},
  lua_ls = {
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
  rust_analyzer = {},
  zls = {},
}

mason_lspconfig.setup {
  ensure_installed = vim.tbl_keys(ensure_installed),
}

mason_lspconfig.setup_handlers {
  function(server_name)
    require("lspconfig")[server_name].setup {
      capabilities = capabilities,
      on_attach = function(_, bufnr)
        vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
        local buffer = { buffer = bufnr }

        -- Vim classic keybindings
        vim.keymap.set("n", "gd", vim.lsp.buf.definition, buffer)
        vim.keymap.set("n", "gD", vim.lsp.buf.declaration, buffer)
        vim.keymap.set("n", "gi", vim.lsp.buf.implementation, buffer)
        vim.keymap.set("n", "gr", vim.lsp.buf.references, buffer)
        vim.keymap.set("n", "R", vim.lsp.buf.rename, buffer)
        vim.keymap.set("n", "K", vim.lsp.buf.hover, buffer)
        vim.keymap.set("n", "gf", vim.lsp.buf.format, buffer)
        vim.keymap.set("n", "gl", vim.diagnostic.open_float, buffer)
        vim.keymap.set("n", "gp", vim.diagnostic.goto_prev, buffer)
        vim.keymap.set("n", "gn", vim.diagnostic.goto_next, buffer)
        vim.keymap.set("n", "C", vim.lsp.buf.code_action, buffer)
        vim.keymap.set("n", "<C-s>", vim.lsp.buf.signature_help, buffer)
        vim.keymap.set("i", "<C-s>", vim.lsp.buf.signature_help, buffer)
      end,
      settings = ensure_installed[server_name],
      handlers = {
        ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" }),
        ["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" }),
      },
    }
  end,
}

vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = { "*.rs", "*.lua" },
  callback = function(_) vim.lsp.buf.format() end,
})

local virtual_text = true
vim.api.nvim_create_user_command("VirtualTextToggle", function()
  virtual_text = not virtual_text
  vim.diagnostic.config {
    virtual_text = virtual_text,
  }
end, {})

vim.diagnostic.config {
  virtual_text = true,
}

setup "fidget"

-- null ls
require("null-ls").setup {
  sources = {
    require("null-ls").builtins.code_actions.gitsigns,

    require("null-ls").builtins.diagnostics.golangci_lint,
    require("null-ls").builtins.diagnostics.trail_space.with { disabled_filetypes = { "NvimTree" } },

    require("null-ls").builtins.formatting.stylua,
    require("null-ls").builtins.formatting.goimports,
  },
}

-- Git
require("gitsigns").setup {
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "_" },
    topdelete = { text = "‾" },
    changedelete = { text = "~" },
  },
}

vim.keymap.set("n", "<leader>gb", function() vim.cmd.Gitsigns "blame_line" end)

vim.api.nvim_create_user_command("Gp", function(_, _) vim.cmd.Git "push" end, {})

vim.keymap.set("n", "<leader>gs", vim.cmd.Git)

-- telescope
require("telescope").setup {}
local no_preview = { previewer = false }
local dropdown = require("telescope.themes").get_dropdown(no_preview)

require("telescope").load_extension "fzf"

vim.keymap.set("n", "<leader><leader>", function() require("telescope.builtin").find_files(no_preview) end)
vim.keymap.set("n", "<leader>ff", function() require("telescope.builtin").find_files(no_preview) end)
vim.keymap.set("n", "<leader>gf", function() require("telescope.builtin").git_files(no_preview) end)
vim.keymap.set("n", "<leader>o", function() require("telescope.builtin").treesitter(dropdown) end)
vim.keymap.set("n", "??", function() require("telescope.builtin").live_grep() end)
vim.keymap.set("n", "<leader>fc", function() require("telescope.builtin").commands() end)
vim.keymap.set("n", "<leader>fh", function() require("telescope.builtin").help_tags(no_preview) end)

-- Edit configurations
vim.keymap.set(
  "n",
  "<leader>fd",
  function() require("telescope.builtin").find_files(vim.tbl_extend("keep", { cwd = "~/dev/dotfiles" }, no_preview)) end
)

-- treesitter
require("nvim-treesitter.configs").setup {
  ensure_installed = { "json", "yaml", "c", "cpp", "lua", "rust", "go", "python", "php" },
  context_commentstring = {
    enable = true,
  },
  highlight = {
    enable = true,
  },
  rainbow = {
    enable = true,
    extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
    max_file_lines = nil, -- Do not enable for files with more than n lines, int
  },
  textobjects = {
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        ["]m"] = "@function.outer",
        ["]]"] = "@class.outer",
      },
      goto_next_end = {
        ["]M"] = "@function.outer",
        ["]["] = "@class.outer",
      },
      goto_previous_start = {
        ["[m"] = "@function.outer",
        ["[["] = "@class.outer",
      },
      goto_previous_end = {
        ["[M"] = "@function.outer",
        ["[]"] = "@class.outer",
      },
    },
    select = {
      enable = true,

      -- Automatically jump forward to textobj, similar to targets.vim
      lookahead = true,

      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
  },
}
pcall(require("nvim-treesitter.install").update { with_sync = true })

-- Toggleterm
require("toggleterm").setup {
  size = function(term)
    if term.direction == "horizontal" then
      return 15
    elseif term.direction == "vertical" then
      return vim.o.columns * 0.4
    end
  end,
  direction = "vertical",
}
vim.keymap.set({ "n", "t" }, "<C-`>", "<cmd>ToggleTerm<CR>", {})

-- Golang
vim.g.go_gopls_enabled = 0
vim.g.go_template_autocreate = 0

setup "tmux"
