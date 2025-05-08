--                             Amirreza Ask's Neovim

--           *Project*                    |         *LSP*
-- <leader>pf -> Project file             |   gd -> Goto to definition
-- <leader>pg -> Project grep             |   gr -> Goto to references
-- <leader>pw -> Project word             |   gi -> Goto to implementation
-- <leader>pW -> Project word with input  |   R  -> Execute rename
-- <leader>ps -> Project (LSP) symbols    |   C  -> Execute code action
-- <leader>pd -> Project Diagnostic       |   K  -> Toggle Hover over symbol
-- ____________________________________   |   L  -> Toggle line diagnostic

--              *Find*
-- <leader>fh -> Find Neovim help tag
-- <leader>ff -> Find file on current buffers directory
-- <leader>fb -> Find buffer

vim.o.wrap = true -- Wrap long lines.
vim.o.breakindent = true -- Indent wrapped lines.
vim.o.signcolumn = "yes" -- Always show signcolumn.
vim.o.swapfile = false -- Disable swapfile.
vim.o.undofile = true -- Store undo history on disk
vim.o.splitbelow = true -- Split windows below the current windows
vim.o.splitright = true -- Split windows right to the current windows
vim.o.clipboard = "unnamedplus" -- Copy/Cut/Paste to system clipboard
vim.o.ignorecase = true -- Search case insensitive...
vim.o.smartcase = true -- ... but not if it contains caps
vim.o.fo = "jcql" -- See :help fo-table
vim.o.updatetime = 100 -- Faster completion
vim.o.laststatus = 3 -- Single Statusline for all windows
vim.o.cursorline = false -- I know where is my cursor.
vim.o.guicursor = "" -- Don't dare to touch my cursor.
vim.o.timeoutlen = 300 -- Faster completion
vim.o.number = true -- Line numbers
vim.o.termguicolors = true -- Enable 24-bit RGB colors
vim.o.inccommand = "split" -- Show partial commands in the command line
vim.o.relativenumber = true -- Relative line numbers
vim.o.scrolloff = 10 -- Scroll when cursor is 8 lines away from screen edge
vim.o.winborder = "rounded"
vim.o.title = true
function _G.titlestring()
  local root = vim.fs.root(vim.fn.getcwd(), ".git")
  root = root or vim.fn.getcwd()
  return root:match("^.+/(.+)$")
end

vim.o.titlestring = "%{v:lua.titlestring()}" -- Set title of the terminal.
vim.o.statusline = "%m%w%q%h%r%f%=[%l :%c]%y"

vim.api.nvim_create_autocmd("TextYankPost", {
  pattern = "*",
  callback = function()
    vim.highlight.on_yank({
      higroup = "IncSearch",
      timeout = 40,
    })
  end,
})
vim.g.mapleader = " "
vim.g.maplocalleader = ","

local map = vim.keymap.set

map("n", "Y", "^v$y", { desc = "Copy whole line" })

-- Ways to escape the INSERT mode
map("t", "<esc>", [[<C-\><C-n>]])
map("i", "jk", "<ESC>")
map("i", "kj", "<ESC>")
map("i", "<C-c>", "<esc>")

-- Moving around will always keep you at the center.
map("n", "<C-d>", "<C-d>zz")
map("n", "<C-u>", "<C-u>zz")
map("n", "n", "nzz")
map("n", "N", "Nzz")

-- Split navigation
map("n", "<C-j>", "<C-w>j")
map("n", "<C-k>", "<C-w>k")
map("n", "<C-h>", "<C-w>h")
map("n", "<C-l>", "<C-w>l")
map("t", "<C-j>", "<C-\\><C-n><C-w>j", { noremap = true })
map("t", "<C-k>", "<C-\\><C-n><C-w>k", { noremap = true })
map("t", "<C-h>", "<C-\\><C-n><C-w>h", { noremap = true })
map("t", "<C-l>", "<C-\\><C-n><C-w>l", { noremap = true })

map("n", "<CR>", "v:hlsearch ? ':nohlsearch<CR>' : '<CR>'", { expr = true, noremap = true })

-- Wrapped lines are just lines.
map("n", "j", "gj")
map("n", "k", "gk")

-- Quickfix list navigation.
map("n", "{", "<cmd>cprev<CR>")
map("n", "}", "<cmd>cnext<CR>")

-- Move code around
map({ "x" }, "<M-j>", ":move '>+1<CR>gv=gv", { noremap = true, silent = true })
map({ "x" }, "<M-k>", ":move '<-2<CR>gv=gv", { noremap = true, silent = true })

-- Fat finger support
vim.cmd [[ command! W w ]]
vim.cmd [[ command! Q q ]]

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
    dependencies = { "folke/tokyonight.nvim", { "rose-pine/neovim", name = "rose-pine" } },
    config = function()
      ---@diagnostic disable-next-line: missing-fields
      require("rose-pine").setup { styles = { italic = false } }
      ---@diagnostic disable-next-line: missing-fields
      vim.g.gruvi_style = "dark"
      vim.cmd.colorscheme("rose-pine")

      -- Make everything transparent.
      vim.cmd [[ 
        hi! Normal      guibg=none
        hi! NormalFloat guibg=none
        hi! FloatBorder guibg=none
        hi! NormalNC    guibg=none
        hi! LineNr      guibg=none
        hi! SignColumn  guibg=none
      ]]
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

  { -- LSP configurations.
    "neovim/nvim-lspconfig",
    dependencies = { { "williamboman/mason.nvim", opts = {} } },
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
          vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
          vim.keymap.set("n", "gr", vim.lsp.buf.references, { buffer = args.buf })
          vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { buffer = args.buf })
          vim.keymap.set("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
          vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
          vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf })
          vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
          vim.keymap.set({ "n", "i" }, "<C-x>", vim.lsp.buf.signature_help, { buffer = args.buf })
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

      vim.keymap.set("n", "<leader>pf", SnacksPicker.files)
      vim.keymap.set("n", "<leader>pg", SnacksPicker.grep)
      vim.keymap.set({ "n", "v" }, "<leader>pw", SnacksPicker.grep_word)
      vim.keymap.set("n", "<leader>pW", grep_input)
      vim.keymap.set("n", "<leader>ps", SnacksPicker.lsp_workspace_symbols)
      vim.keymap.set("n", "<leader>pd", SnacksPicker.diagnostics)

      vim.keymap.set("n", "<leader>fh", SnacksPicker.help)

      -- LSP
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
