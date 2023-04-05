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
vim.opt.guicursor = ""
vim.opt.shortmess:append "c" -- Don't pass messages to |ins-completion-menu|.
vim.opt.shortmess:append "I" -- No Intro message
vim.opt.clipboard:append "unnamedplus" -- use system clipboard as default register.
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.cursorline = true
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.g.mapleader = " "
-- Plugins
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
  -- Colorschemes [[[
  { "folke/tokyonight.nvim" },
  { "rose-pine/neovim",        name = "rose-pine" },
  { "catppuccin/nvim",         name = "catppuccin" },
  { "Mofiqul/dracula.nvim" },
  { "ellisonleao/gruvbox.nvim" },
  -- ]]]

  { "numToStr/Comment.nvim",   opts = {} }, -- Comment

  { -- telescope
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim", { "nvim-telescope/telescope-fzf-native.nvim", build = "make" } },
    config = function()
      require("telescope").setup {}
      require("telescope").load_extension "fzf"
    end,
  },
  { -- Treesitter, incremental, fast parsing
    "nvim-treesitter/nvim-treesitter",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects", "nvim-treesitter/playground" },
    config = function()
      require("nvim-treesitter.configs").setup {
        ensure_installed = { "json", "yaml", "c", "cpp", "lua", "rust", "go", "python", "php" },
        context_commentstring = { enable = true },
        highlight = { enable = true },
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
      pcall(require("nvim-treesitter.install").update { with_sync = true })
    end,
  },
  { -- LSP: Language server configs + Auto install LSPs
    "neovim/nvim-lspconfig",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "folke/neodev.nvim",
      { "j-hui/fidget.nvim", opts = {} },
    },
    config = function()
      require("mason").setup {}
      for _, pkg in ipairs { "stylua", "golangci-lint", "goimports", "yamlfmt" } do -- ensure these tools are installed
        if not require("mason-registry").is_installed(pkg) then require("mason.api.command").MasonInstall { pkg } end
      end

      local mason_lspconfig = require "mason-lspconfig"
      local lsp_servers = {
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
        ensure_installed = vim.tbl_keys(lsp_servers),
      }

      mason_lspconfig.setup_handlers {
        function(server_name)
          require("lspconfig")[server_name].setup {
            settings = lsp_servers[server_name],
            handlers = {
              ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" }),
              ["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" }),
            },
          }
        end,
      }

      -- Auto format
      vim.api.nvim_create_autocmd("BufWritePre", {
        pattern = { "*.rs", "*.lua" },
        callback = function(_) vim.lsp.buf.format() end,
      })
    end,
  },

  {
    "hrsh7th/nvim-cmp", -- Autocompletion popup
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-vsnip",
      "hrsh7th/vim-vsnip",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-buffer",
    },
    config = function()
      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)
      local cmp = require "cmp"
      cmp.setup {
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
    end,
  },
  {
    "jose-elias-alvarez/null-ls.nvim",
    config = function()
      require("null-ls").setup {
        sources = {
          require("null-ls").builtins.code_actions.gitsigns,

          require("null-ls").builtins.diagnostics.golangci_lint,
          require("null-ls").builtins.diagnostics.trail_space.with { disabled_filetypes = { "NvimTree" } },

          require("null-ls").builtins.formatting.stylua,
          require("null-ls").builtins.formatting.goimports,
        },
      }
    end,
  }, -- Adapt third party tools as LSP servers.
  { "stevearc/oil.nvim",    opt = {} }, -- File manager like a BOSS
  { "pbrisbin/vim-mkdir" }, -- Automatically create directory if not exists
  { "fladson/vim-kitty" }, -- Support Kitty terminal config syntax
  { "towolf/vim-helm" }, -- Support for helm template syntax
  { "tpope/vim-surround" }, -- surrounding text objects
  { "kevinhwang91/nvim-bqf" }, -- Preview quickfix list item.
  { "tpope/vim-eunuch" }, -- Helper commands like :Rename, :Move, :Delete, :Remove, ...
  { "tpope/vim-sleuth" }, -- Heuristically set buffer options
  { "windwp/nvim-autopairs" }, -- Auto insert pairs like () [] {}
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      local gs = require "gitsigns"
      gs.setup {
        signs = {
          add = { text = "+" },
          change = { text = "~" },
          delete = { text = "_" },
          topdelete = { text = "‾" },
          changedelete = { text = "~" },
        },
      }
    end,
  }, -- Signs next to line numbers to show git status of a line
  {
    "tpope/vim-fugitive",
    config = function()
      vim.api.nvim_create_user_command("Gp", function(_, _) vim.cmd.Git "push" end, {})
    end,
  }, -- Best Git Client after magit :)
  { "dag/vim-fish" }, -- Vim fish syntax
  { "jansedivy/jai.vim" }, -- Jai from Jonathan Blow
  {
    "akinsho/toggleterm.nvim",
    config = function(_, _)
      require("toggleterm").setup {
        size = 20,
        direction = "horizontal",
      }
    end,
  },
}

-- Colorschemes
pcall(vim.cmd.colorscheme, "tokyonight-night")
vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
vim.api.nvim_set_hl(0, "NormalNC", { bg = "none" })

-- Keybindings
local no_preview = { previewer = false }
local dropdown = require("telescope.themes").get_dropdown(no_preview)
vim.keymap.set("n", "<leader>v", "<cmd>vsplit<CR>")
vim.keymap.set("n", "<leader>h", "<cmd>split<CR>")
vim.keymap.set("n", "<leader><leader>", function() require("telescope.builtin").git_files(no_preview) end)
vim.keymap.set("n", "<leader>ff", function() require("telescope.builtin").find_files(no_preview) end)
vim.keymap.set("n", "<C-p>", function() require("telescope.builtin").git_files(no_preview) end)
vim.keymap.set("n", "<leader>k", function() require("telescope.builtin").current_buffer_fuzzy_find(no_preview) end)
vim.keymap.set("n", "<leader>o", function() require("telescope.builtin").treesitter(dropdown) end)
vim.keymap.set("n", "??", function() require("telescope.builtin").live_grep() end)
vim.keymap.set("n", "<leader>gs", vim.cmd.Git)
vim.keymap.set("n", "<leader>gl", function() require("gitsigns").blame_line { full = true } end)
vim.keymap.set("n", "<leader>gd", function() require("gitsigns").diffthis "~" end)
vim.keymap.set("n", "<leader>gP", function() vim.cmd.Git "push" end)
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
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
vim.keymap.set("n", "<A-l>", "<C-w><")
vim.keymap.set("n", "<A-h>", "<C-w>>")
vim.keymap.set("n", "<A-j>", "<C-w>-")
vim.keymap.set("n", "<A-k>", "<C-w>+")
vim.keymap.set("t", "<A-l>", "<C-\\><C-n><C-w><")
vim.keymap.set("t", "<A-h>", "<C-\\><C-n><C-w>>")
vim.keymap.set("t", "<A-j>", "<C-\\><C-n><C-w>-")
vim.keymap.set("t", "<A-k>", "<C-\\><C-n><C-w>+")
vim.keymap.set({ "n", "t" }, "<leader>j", "<cmd>ToggleTerm<CR>")
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local bufnr = args.buf
    vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
    local buffer = { buffer = bufnr }
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
})
