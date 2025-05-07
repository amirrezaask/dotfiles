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
vim.keymap.set("n", "<C-q>", function()
  local wins = vim.api.nvim_list_wins()
  for _, win in ipairs(wins) do
    local buf = vim.api.nvim_win_get_buf(win)
    if vim.api.nvim_get_option_value("buftype", { buf = buf }) == "quickfix" then
      vim.cmd.cclose()
      return
    end
  end
  vim.cmd.copen()
end)

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

vim.o.rtp = vim.o.rtp .. "," .. lazypath

require("lazy").setup({
  {
    "amirrezaask/nvim-gruvbuddy.lua", -- Colorscheme, inspired by great @tjdevries.
    dependencies = {
      {
        "rose-pine/neovim",
        name = "rose-pine",
        opts = {
          styles = {
            italic = false,
            transparency = true,
          },
        },
      },
      { "folke/tokyonight.nvim" },
    },

    config = function()
      vim.g.gruvbuddy_style = "dark"

      vim.cmd.colorscheme("tokyonight-night")
    end,
  },

  "tpope/vim-sleuth", -- Configure indentation based on current indentation of the file.

  { -- TODO: Highlight TODO and FIXMEs.
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = { signs = false },
  },

  { -- My custom crafted statusline plugin
    "amirrezaask/nvim-statusline.lua",
    dir = "~/src/nvim-statusline.lua",
    opts = {},
  },

  {
    "echasnovski/mini.nvim",
    config = function()
      require("mini.ai").setup()
      require("mini.comment").setup()
      require("mini.move").setup() -- M-h M-j M-k M-l move code blocks around.
      require("mini.icons").setup()
    end,
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
          vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
          vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { buffer = args.buf })
          vim.keymap.set("n", "gr", vim.lsp.buf.references, { buffer = args.buf })
          vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { buffer = args.buf })
          vim.keymap.set("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
          vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
          vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
          vim.keymap.set("n", "<leader>s", vim.lsp.buf.signature_help, { buffer = args.buf })
          vim.keymap.set("n", "<leader>l", vim.diagnostic.open_float, { buffer = args.buf })
          vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { buffer = args.buf })
        end,
      })

      vim.lsp.enable({ "gopls", "intelephense", "lua_ls", "ocamllsp" })

      vim.diagnostic.config({ virtual_text = true })
    end,
  },

  -- { "nvim-tree/nvim-web-devicons" }, -- Icons in terminal, nice.

  { "supermaven-inc/supermaven-nvim", opts = {} }, -- Best usage for AI.

  { "MagicDuck/grug-far.nvim", opts = {} }, -- Find/Replace project wide.

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
    config = function()
      require("snacks").setup {
        picker = { enabled = true },
        bigfile = { enabled = true },
        termainal = { enabled = true },
      }

      Snacks = require("snacks")
      SnacksPicker = Snacks.picker

      vim.keymap.set("n", "<leader><leader>", SnacksPicker.files, { desc = "Find Files" })
      vim.keymap.set("n", "<leader>ff", function()
        local root =
          vim.fs.find(".git", { upward = true, type = "directory", path = vim.fn.expand("%:h:p") })[1]:sub(1, -5)
        SnacksPicker.files { cwd = root }
      end, { desc = "Find Files" })
      vim.keymap.set("n", "<leader>b", SnacksPicker.buffers, { desc = "Find Buffers" })
      vim.keymap.set("n", "<leader>h", SnacksPicker.help, { desc = "Vim Help Tags" })
      vim.keymap.set("n", "<C-p>", SnacksPicker.git_files, { desc = "Git Files" })
      vim.keymap.set("n", "??", SnacksPicker.grep, { desc = "Live Grep" })
      vim.keymap.set("v", "??", SnacksPicker.grep_word, { desc = "Grep word under cursor" })
      vim.keymap.set("n", "?s", function()
        vim.ui.input({ prompt = "Grep word: " }, function(input)
          if input == "" or input == nil then
            return
          end
          SnacksPicker.grep_word({
            search = function(_)
              return input
            end,
          })
        end)
      end, { desc = "Grep" })
      vim.keymap.set("n", "<leader>o", SnacksPicker.lsp_symbols, { desc = "LSP Document Symbols" })
      vim.keymap.set("n", "<leader>O", SnacksPicker.lsp_workspace_symbols, { desc = "LSP Workspace Symbols" })
      vim.keymap.set("n", "<leader>fd", function()
        SnacksPicker.files({ cwd = "~/.dotfiles" })
      end, { desc = "Find Dotfiles" })

      vim.lsp.buf.definition = SnacksPicker.lsp_definitions
      vim.lsp.buf.implementation = SnacksPicker.lsp_implementations
      vim.lsp.buf.references = SnacksPicker.lsp_references
      vim.lsp.buf.type_definition = SnacksPicker.lsp_type_definitions

      vim.keymap.set({ "n", "t" }, "<C-s>", Snacks.terminal.toggle, { desc = "Terminal" }) -- Terminal
    end,
  },
  { -- FZF: currently i am using snacks.picker but this will be here just as a fallback.
    "ibhagwan/fzf-lua",
    enabled = false,
    config = function()
      Fzf = require("fzf-lua")
      Fzf.setup {
        fzf_colors = true,
        keymap = {
          fzf = {
            ["ctrl-q"] = "select-all+accept", -- Select all items and send to quickfix
          },
        },
      }

      vim.api.nvim_set_hl(0, "FzfLuaNormal", { link = "NormalFloat" })
      vim.api.nvim_set_hl(0, "FzfLuaBorder", { link = "NormalFloat" })

      Fzf.register_ui_select()

      vim.keymap.set("n", "<leader><leader>", Fzf.files, { desc = "Find Files" })
      vim.keymap.set("n", "<leader>b", Fzf.buffers, { desc = "Find Buffers" })
      vim.keymap.set("n", "<leader>h", Fzf.helptags, { desc = "Vim Help Tags" })
      vim.keymap.set("n", "<C-p>", Fzf.git_files, { desc = "Git Files" })
      vim.keymap.set("n", "??", Fzf.live_grep, { desc = "Live Grep" })
      vim.keymap.set("n", "<leader>fs", Fzf.grep, { desc = "Grep" })
      vim.keymap.set("v", "??", Fzf.grep_cword, { desc = "Grep word under cursor" })
      vim.keymap.set("n", "<leader>o", Fzf.lsp_document_symbols, { desc = "LSP Document Symbols" })
      vim.keymap.set("n", "<leader>O", Fzf.lsp_live_workspace_symbols, { desc = "LSP Workspace Symbols" })
      vim.keymap.set("n", "<M-o>", Fzf.lsp_live_workspace_symbols, { desc = "LSP Workspace Symbols" })
      vim.keymap.set("n", "<leader>fd", function()
        Fzf.files({ cwd = "~/.dotfiles" })
      end, { desc = "Find Dotfiles" })

      vim.lsp.buf.definition = Fzf.lsp_definitions
      vim.lsp.buf.implementation = Fzf.lsp_implementations
      vim.lsp.buf.references = Fzf.lsp_references
      vim.lsp.buf.type_definition = Fzf.lsp_type_definitions
    end,
  }, -- as a fallback for snacks picker.

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
