-- Amirreza Ask's Neovim
-- Last IDE you'll ever need.

-- Project Keys:
-- <leader>pf  Project file
-- <leader>pg  Project git file
-- <leader>ps  Project search (grep)
-- <leader>pw  Project word
-- <leader>pW  Project word with input
-- <leader>ps  Project (LSP) symbols
-- <leader>pd  Project Diagnostic

-- Docuemnt Keys:
-- <leader>ds Document symbol
-- <leader>dd Document diagnostics

-- LSP Keys:
-- <leader>ld  Jump to definition
-- <leader>lD  Jump to declaration
-- <leader>lr  Jump to references
-- <leader>li  Jump to implementation
-- <leader>R   Execute rename
-- <leader>C   Execute code action
--     K       Toggle Hover over symbol
--     L       Toggle line diagnostic

--   Vim:
-- <leader>vb  Find Buffer
-- <leader>vh  Find help tag

vim.o.wrap = true -- Wrap long lines.
vim.o.breakindent = true -- Indent wrapped lines.
vim.o.signcolumn = "yes" -- Show signcolumn.
vim.o.swapfile = false -- Disable swapfile.
vim.o.undofile = true -- Store undo history on disk
vim.o.splitbelow = true -- Split windows below the current windows
vim.o.splitright = true -- Split windows right to the current windows
vim.o.showmode = false -- Don't show Vim mode in the command line.
vim.o.clipboard = "unnamedplus" -- Copy/Cut/Paste to system clipboard
vim.o.ignorecase = true -- Search case insensitive...
vim.o.smartcase = true -- ... but not if it contains caps
vim.o.cursorline = true -- Highlight current line
vim.o.guicursor = vim.o.guicursor .. ",t:ver25"
vim.o.fo = "jcql" -- See :help fo-table
vim.o.updatetime = 100 -- Faster completion
vim.o.laststatus = 3 -- Single Statusline for all windows
vim.o.timeoutlen = 300 -- Faster completion
vim.o.number = true -- Line numbers
vim.o.termguicolors = true -- Enable 24-bit RGB colors
vim.o.inccommand = "split" -- Show partial commands in the command line
vim.o.relativenumber = true -- Relative line numbers
vim.o.scrolloff = 10 -- Scroll when cursor is 8 lines away from screen edge
-- vim.o.list = true -- Show whitespace
-- vim.o.listchars = "tab:  ,trail:·,extends: ,precedes: ,eol:↲,conceal:┊,nbsp:␣"
vim.o.winborder = "rounded"
vim.o.title = true
function _G.titlestring()
  local root = vim.fs.root(vim.fn.getcwd(), ".git")
  root = root or vim.fn.getcwd()
  return root:match("^.+/(.+)$")
end

vim.o.titlestring = "%M%{v:lua.titlestring()}" -- Set title of the terminal.

vim.g.mapleader = " "
vim.g.maplocalleader = ","

vim.keymap.set("n", "Y", "^v$y", { desc = "Copy whole line" })
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set({ "n" }, "<C-j>", "<C-w>j") -- Window navigation
vim.keymap.set({ "n" }, "<C-k>", "<C-w>k") -- Window navigation
vim.keymap.set({ "n" }, "<C-h>", "<C-w>h") -- Window navigation
vim.keymap.set({ "n" }, "<C-l>", "<C-w>l") -- Window navigation
vim.keymap.set({ "t" }, "<C-j>", "<C-\\><C-n><C-w>j", { noremap = true }) -- Window navigation
vim.keymap.set({ "t" }, "<C-k>", "<C-\\><C-n><C-w>k", { noremap = true }) -- Window navigation
vim.keymap.set({ "t" }, "<C-h>", "<C-\\><C-n><C-w>h", { noremap = true }) -- Window navigation
vim.keymap.set({ "t" }, "<C-l>", "<C-\\><C-n><C-w>l", { noremap = true }) -- Window navigation
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
vim.keymap.set("n", "<CR>", "v:hlsearch ? ':nohlsearch<CR>' : '<CR>'", { expr = true, noremap = true })
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("n", "{", "<cmd>cprev<CR>")
vim.keymap.set("n", "}", "<cmd>cnext<CR>")
vim.keymap.set({ "n", "t" }, "<M-k>", "<cmd>wincmd q<CR>")
vim.keymap.set({ "x" }, "<M-j>", ":move '>+1<CR>gv=gv", { noremap = true, silent = true }) -- Moves ...
vim.keymap.set({ "x" }, "<M-k>", ":move '<-2<CR>gv=gv", { noremap = true, silent = true }) -- ... code around

-- Lazy package manager initialization
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end

vim.o.rtp = vim.o.rtp .. "," .. lazypath -- Add lazy.nvim to runtimepath

require("lazy").setup({
  {
    "amirrezaask/gruvi.nvim", -- Colorscheme, inspired by great @tjdevries's gruvbuddy.nvim
    dir = "~/src/github/gruvi.nvim",
    config = function()
      vim.g.gruvi_style = "dark"
      vim.cmd.colorscheme("gruvi")
    end,
  },

  "tpope/vim-sleuth", -- Configure indentation based on current indentation of the file.

  { -- Git signs
    "lewis6991/gitsigns.nvim",
    opts = {
      signs = {
        add = { text = "+" },
        change = { text = "~" },
        delete = { text = "_" },
        topdelete = { text = "‾" },
        changedelete = { text = "~" },
      },
    },
  },

  { -- My custom crafted statusline plugin
    "amirrezaask/vitaline.nvim",
    opts = {},
    dependencies = { "nvim-tree/nvim-web-devicons" },
  },

  { -- LSP configurations.
    "neovim/nvim-lspconfig",
    config = function()
      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function(args)
          vim.keymap.set("n", "[[", function()
            vim.diagnostic.jump({ count = -1 })
          end, { buffer = args.buf })
          vim.keymap.set("n", "]]", function()
            vim.diagnostic.jump({ count = 1 })
          end, { buffer = args.buf })
          vim.keymap.set("n", "C-]", vim.lsp.buf.definition, { buffer = args.buf })
          vim.keymap.set("n", "<leader>ld", vim.lsp.buf.definition, { buffer = args.buf })
          vim.keymap.set("n", "<leader>lD", vim.lsp.buf.declaration, { buffer = args.buf })
          vim.keymap.set("n", "<leader>lr", vim.lsp.buf.references, { buffer = args.buf })
          vim.keymap.set("n", "<leader>li", vim.lsp.buf.implementation, { buffer = args.buf })
          vim.keymap.set("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
          vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
          vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf })
          vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
          vim.keymap.set("n", "<leader>s", vim.lsp.buf.signature_help, { buffer = args.buf })
        end,
      })

      vim.lsp.enable({ "gopls", "intelephense", "lua_ls", "ocamllsp" })

      vim.diagnostic.config({ virtual_text = true })
    end,
  },

  { "supermaven-inc/supermaven-nvim", opts = {} }, -- Best usage for AI.

  { -- File management done right.
    "stevearc/oil.nvim",
    config = function()
      require("oil").setup {}
      vim.keymap.set("n", "-", "<cmd>Oil<CR>")
    end,
  },

  { "williamboman/mason.nvim", opts = {} }, -- Package manager for your system inside neovim.

  { -- Blazingly fast autocomplete
    "saghen/blink.cmp",
    tag = "v1.1.1",
    dependencies = { { "folke/lazydev.nvim", opts = { library = {} } } }, -- Better neovim development support. },
    opts = {
      keymap = { preset = "enter" },
      cmdline = { enabled = false },
      sources = {
        default = { "lsp", "path", "snippets", "lazydev" },
        providers = {
          lazydev = { module = "lazydev.integrations.blink", score_offset = 100 },
        },
      },
    },
  },

  { -- Autoformat/fixes
    "stevearc/conform.nvim",
    config = function()
      require("conform").setup({
        formatters_by_ft = {
          lua = { "stylua" },
          go = { "goimports" },
          ocmal = { "ocamlformat" },
          php = {},
        },
      })
      local autoformat_languages = { "*.lua", "*.go", "*.ocmal" }

      vim.api.nvim_create_autocmd("BufWritePre", {
        pattern = autoformat_languages,
        callback = function(args)
          require("conform").format({ bufnr = args.buf })
        end,
      })
    end,
  },

  { -- Collection of plugins by folkee.
    "folke/snacks.nvim",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
    },
    config = function()
      require("snacks").setup {
        picker = { enabled = true },
        bigfile = { enabled = true },
        termainal = { enabled = true },
      }

      Snacks = require("snacks")
      SnacksPicker = Snacks.picker

      local grep_input = function()
        vim.ui.input({ prompt = "Grep> " }, function(input)
          if input == "" or input == nil then
            return
          end
          SnacksPicker.grep_word({
            search = function(_)
              return input
            end,
          })
        end)
      end

      vim.keymap.set("n", "<C-p>", SnacksPicker.git_files)

      -- Project Keys
      vim.keymap.set("n", "<leader>pf", SnacksPicker.files)
      vim.keymap.set("n", "<leader>pg", SnacksPicker.grep)
      vim.keymap.set({ "n", "v" }, "<leader>pw", SnacksPicker.grep_word)
      vim.keymap.set("n", "<leader>pW", grep_input)
      vim.keymap.set("n", "<leader>ps", SnacksPicker.lsp_workspace_symbols)
      vim.keymap.set("n", "<leader>pd", SnacksPicker.diagnostics)

      -- Document Keys
      vim.keymap.set("n", "<leader>ds", SnacksPicker.lsp_symbols)
      vim.keymap.set("n", "<leader>dd", SnacksPicker.diagnostics_buffer)

      -- Vim
      vim.keymap.set("n", "<leader>vb", SnacksPicker.buffers)
      vim.keymap.set("n", "<leader>vh", SnacksPicker.help)

      vim.lsp.buf.definition = SnacksPicker.lsp_definitions
      vim.lsp.buf.implementation = SnacksPicker.lsp_implementations
      vim.lsp.buf.references = SnacksPicker.lsp_references
      vim.lsp.buf.type_definition = SnacksPicker.lsp_type_definitions

      vim.keymap.set({ "n", "t" }, "<C-s>", Snacks.terminal.toggle, { desc = "Terminal" }) -- Terminal
    end,
  },

  { -- Treesitter
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
    main = "nvim-treesitter.configs", -- Sets main module to use for opts
    opts = {
      ensure_installed = "all",
      auto_install = true,
      highlight = { enable = true },
      indent = { enable = true, disable = { "ruby" } },
    },
  },
})
