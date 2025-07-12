vim.g.mapleader = " " -- <leader> in keybindings means Space.
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
vim.o.formatoptions = "jcql" -- See :help fo-table
vim.o.updatetime = 100 -- Faster completion
vim.o.laststatus = 3 -- Single Statusline for all windows
vim.o.timeoutlen = 300 -- Faster completion
vim.o.number = true -- Line numbers
vim.o.termguicolors = true -- Enable 24-bit RGB colors
vim.o.inccommand = "split" -- Show partial commands in the command line
vim.o.relativenumber = true -- Relative line numbers
vim.o.scrolloff = 10 -- Scroll when cursor is 8 lines away from screen edge
vim.o.statusline = "%m%w%q%h%r%f%=[%l :%c]%y"
vim.o.title = true -- Sets title of the terminal window to current project name.
vim.o.titlestring = [[ %{v:lua.vim.fs.basename(finddir(getcwd(),'.git'))} ]]
vim.o.guicursor = "" -- Don't tinker with the cursor.
-- Y yanks whole line.
vim.keymap.set("n", "Y", "^v$y", { desc = "Copy whole line" })
-- Ways to escape the INSERT mode
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
vim.keymap.set("i", "<C-c>", "<esc>")
-- Moving around will always keep you at the center.
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
-- Split navigation
vim.keymap.set("n", "<C-j>", "<C-w>j")
vim.keymap.set("n", "<C-k>", "<C-w>k")
vim.keymap.set("n", "<C-h>", "<C-w>h")
vim.keymap.set("n", "<C-l>", "<C-w>l")
vim.keymap.set("t", "<C-j>", "<C-\\><C-n><C-w>j", { noremap = true })
vim.keymap.set("t", "<C-k>", "<C-\\><C-n><C-w>k", { noremap = true })
vim.keymap.set("t", "<C-h>", "<C-\\><C-n><C-w>h", { noremap = true })
vim.keymap.set("t", "<C-l>", "<C-\\><C-n><C-w>l", { noremap = true })
-- Disable inc-search on <CR>
vim.keymap.set("n", "<CR>", "v:hlsearch ? ':nohlsearch<CR>' : '<CR>'", { expr = true, noremap = true })
-- Wrapped lines are just lines.
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
-- Quickfix list navigation.
vim.keymap.set("n", "{", "<cmd>cprev<CR>")
vim.keymap.set("n", "}", "<cmd>cnext<CR>")
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
  { -- Colorscheme
    "eemed/sitruuna.vim",
    config = function()
      vim.cmd.colorscheme("sitruuna")
    end,
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
      { "mason-org/mason-lspconfig.nvim", opts = { ensure_installed = { "gopls", "intelephense", "lua_ls" } } },
    },
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
          vim.keymap.set({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, { buffer = args.buf })
        end,
      })
      vim.diagnostic.config({ virtual_text = true })
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
        default = { "lsp", "path", "snippets" },
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
          php = {},
        },
      })
      vim.api.nvim_create_autocmd("BufWritePre", {
        pattern = { "*.lua", "*.go", "*.ocmal" },
        callback = function(args)
          require("conform").format({ bufnr = args.buf })
        end,
      })
    end,
  },
  {
    "ibhagwan/fzf-lua",
    config = function()
      Fzf = require("fzf-lua")
      Fzf.setup {
        "fzf-vim", -- setup similar to fzf.vim
        keymap = { fzf = { ["ctrl-q"] = "select-all+accept" } },
      }
      vim.api.nvim_set_hl(0, "FzfLuaNormal", { link = "NormalFloat" })
      vim.api.nvim_set_hl(0, "FzfLuaBorder", { link = "NormalFloat" })
      Fzf.register_ui_select()
      vim.keymap.set("n", "<leader><leader>", Fzf.files, { desc = "Find Files" })
      vim.keymap.set("n", "<leader>pf", Fzf.git_files, { desc = "Find Files" })
      vim.keymap.set("n", "<leader>ph", Fzf.helptags, { desc = "Vim Help Tags" })
      vim.keymap.set("n", "<leader>pg", Fzf.live_grep, { desc = "Live Grep" })
      vim.keymap.set("n", "<leader>pw", Fzf.grep, { desc = "Grep word" })
      vim.keymap.set("v", "<leader>pw", Fzf.grep_cword, { desc = "Grep <cword>" })
      vim.keymap.set("n", "<leader>ps", Fzf.lsp_document_symbols, { desc = "LSP Document Symbols" })
      vim.keymap.set("n", "<leader>pS", Fzf.lsp_live_workspace_symbols, { desc = "LSP Workspace Symbols" })

      vim.lsp.buf.definition = Fzf.lsp_definitions
      vim.lsp.buf.implementation = Fzf.lsp_implementations
      vim.lsp.buf.references = Fzf.lsp_references
      vim.lsp.buf.type_definition = Fzf.lsp_type_definitions
    end,
  },
})
