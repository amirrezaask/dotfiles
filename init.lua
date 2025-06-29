local map = vim.keymap.set
local autocmd = vim.api.nvim_create_autocmd
local o, g = vim.o, vim.g

g.mapleader = " "
g.maplocalleader = ","

o.wrap = true -- Wrap long lines.
o.breakindent = true -- Indent wrapped lines.
o.signcolumn = "yes" -- Always show signcolumn.
o.swapfile = false -- Disable swapfile.
o.undofile = true -- Store undo history on disk
o.splitbelow = true -- Split windows below the current windows
o.splitright = true -- Split windows right to the current windows
o.clipboard = "unnamedplus" -- Copy/Cut/Paste to system clipboard
o.ignorecase = true -- Search case insensitive...
o.smartcase = true -- ... but not if it contains caps
o.formatoptions = "jcql" -- See :help fo-table
o.updatetime = 100 -- Faster completion
o.laststatus = 3 -- Single Statusline for all windows
o.timeoutlen = 300 -- Faster completion
o.number = true -- Line numbers
o.termguicolors = true -- Enable 24-bit RGB colors
o.inccommand = "split" -- Show partial commands in the command line
o.relativenumber = true -- Relative line numbers
o.scrolloff = 10 -- Scroll when cursor is 8 lines away from screen edge
-- o.winborder = "rounded" -- Floating window borders.
o.statusline = "%m%w%q%h%r%f%=[%l :%c]%y"
-- o.guicursor = ""
-- o.cursorline = true
-- Sets title of the terminal window to current project name.
o.title = true
o.titlestring = [[ %{v:lua.vim.fs.basename(finddir(getcwd(),'.git'))} ]]

autocmd({ "ColorScheme" }, { -- All colorschemes become transparent no matter what.
  callback = function()
    vim.cmd [[
            hi! Normal      guibg=none
            hi! NormalFloat guibg=none
            hi! FloatBorder guibg=none
            hi! NormalNC    guibg=none
            hi! LineNr      guibg=none
            hi! SignColumn  guibg=none
            hi! StatusLine  guibg=none
        ]]
  end,
})

autocmd("TextYankPost", { -- Always transparent.
  pattern = "*",
  callback = function()
    vim.highlight.on_yank({
      higroup = "IncSearch",
      timeout = 40,
    })
  end,
})

-- Y yanks whole line.
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
    "vague2k/vague.nvim",
    config = function()
      require("vague").setup {
        italic = true,
      }

      vim.cmd.colorscheme("vague")
    end,
  },

  { -- Git
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

  { -- Treesitter
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
    main = "nvim-treesitter.configs",
    opts = {
      ensure_installed = { "go", "php", "bash" },
      auto_install = true,
      highlight = { enable = true },
      indent = { enable = true, disable = { "ruby" } },
    },
  },

  "tpope/vim-sleuth", -- Configure indentation based on current indentation of the file.

  { -- LSP configurations.
    "neovim/nvim-lspconfig",
    dependencies = {
      { "mason-org/mason.nvim", opts = {} },
      {
        "mason-org/mason-lspconfig.nvim",
        opts = { ensure_installed = { "gopls", "intelephense", "lua_ls" } },
      },
    },
    config = function()
      autocmd("LspAttach", {
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
          vim.keymap.set({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, { buffer = args.buf })
        end,
      })
      vim.diagnostic.config({ virtual_text = true })
    end,
  },

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
      completion = { list = { selection = { preselect = false } } },
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

      autocmd("BufWritePre", {
        pattern = autoformat_languages,
        callback = function(args)
          require("conform").format({ bufnr = args.buf })
        end,
      })
    end,
  },
  { -- Collection of plugins by folkee.
    "folke/snacks.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("snacks").setup {
        picker = {
          prompt = "> ",
          enabled = true,
          ---@diagnostic disable-next-line: assign-type-mismatch
          layout = { preview = false, preset = "telescope" },
        },
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

      -- <leader> [p]ick [s]omething
      vim.keymap.set("n", "<leader><leader>", SnacksPicker.files)
      vim.keymap.set("n", "<leader>pf", SnacksPicker.files)
      vim.keymap.set("n", "<leader>pF", SnacksPicker.git_files)
      vim.keymap.set("n", "<leader>pg", SnacksPicker.grep)
      vim.keymap.set({ "n", "v" }, "<leader>pw", SnacksPicker.grep_word)
      vim.keymap.set("n", "<leader>pW", grep_input)
      vim.keymap.set("n", "<leader>ps", SnacksPicker.lsp_symbols)
      vim.keymap.set("n", "<leader>pS", SnacksPicker.lsp_workspace_symbols)
      vim.keymap.set("n", "<leader>pd", SnacksPicker.diagnostics)
      vim.keymap.set("n", "<leader>ph", SnacksPicker.help)
      vim.keymap.set("n", "<leader>pc", function()
        SnacksPicker.files { cwd = "~/src/github/dotfiles" }
      end)
      vim.keymap.set("n", "<leader>pp", SnacksPicker.pickers)

      vim.lsp.buf.definition = SnacksPicker.lsp_definitions
      vim.lsp.buf.implementation = SnacksPicker.lsp_implementations
      vim.lsp.buf.references = SnacksPicker.lsp_references
      vim.lsp.buf.type_definition = SnacksPicker.lsp_type_definitions

      vim.keymap.set({ "n", "t" }, "<C-t>", Snacks.terminal.toggle, { desc = "Terminal" }) -- Terminal
    end,
  },
})
