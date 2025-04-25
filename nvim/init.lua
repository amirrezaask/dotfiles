local g, o, keymap = vim.g, vim.o, vim.keymap.set

g.mapleader = " "
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

o.rtp = o.rtp .. "," .. lazypath

require("lazy").setup({
  -- Colorschemes
  { "folke/tokyonight.nvim" },
  { "rose-pine/neovim", name = "rose-pine" },
  { "amirrezaask/nvim-gruvbuddy.lua" },
  { "amirrezaask/nvim-norcalli.lua" },

  { -- Help with neovim/lua dev.
    "folke/lazydev.nvim",
    ft = "lua",
    cmd = "LazyDev",
    opts = {
      library = {
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
  },

  { -- Blazingly fast autocomplete
    "saghen/blink.cmp",
    tag = "v1.1.1",
    opts = {
      keymap = { preset = "enter" },
      cmdline = { enabled = false },
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
        pattern = { "*.lua", "*.go" },
        callback = function(args)
          require("conform").format({ bufnr = args.buf })
        end,
      })
    end,
  },

  { -- Package manager for your system inside neovim.
    "williamboman/mason.nvim",
    opts = {},
  },

  { "kevinhwang91/nvim-bqf", opts = {} },

  {
    "ibhagwan/fzf-lua",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
    },
    config = function()
      Fzf = require("fzf-lua")
      Fzf.setup {
        hls = {
          normal = "NormalFloat",
        },
        files = {
          -- previewer = false,
        },
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
      vim.keymap.set("n", "<leader>fw", Fzf.grep, { desc = "Grep" })
      vim.keymap.set("v", "??", Fzf.grep_cword, { desc = "Grep word under cursor" })
      vim.keymap.set("n", "<leader>o", Fzf.lsp_document_symbols, { desc = "LSP Document Symbols" })
      vim.keymap.set("n", "<leader>O", Fzf.lsp_live_workspace_symbols, { desc = "LSP Workspace Symbols" })
      vim.keymap.set("n", "<M-o>", Fzf.lsp_live_workspace_symbols, { desc = "LSP Workspace Symbols" })
      vim.keymap.set("n", "<leader>fd", function()
        Fzf.files({ cwd = "~/.dotfiles" })
      end, { desc = "Find Dotfiles" })
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
    config = function()
      require("nvim-treesitter.configs").setup {
        ensure_installed = "all",
        highlight = { enable = true },
        sync_install = false,
        auto_install = true,
        ignore_install = {},
        modules = {},
        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            keymaps = {
              ["af"] = "@function.outer",
              ["if"] = "@function.inner",
              ["ac"] = "@class.outer",
              ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
              ["as"] = { query = "@local.scope", query_group = "locals", desc = "Select language scope" },
            },
            selection_modes = {
              ["@parameter.outer"] = "v", -- charwise
              ["@function.outer"] = "V", -- linewise
              ["@class.outer"] = "<c-v>", -- blockwise
            },
            include_surrounding_whitespace = true,
          },
        },
      }
    end,
  },

  {
    "stevearc/oil.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {},
  },

  {
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

  { -- AI Apocalypse
    "supermaven-inc/supermaven-nvim",
    opts = {},
  },

  { -- Find/Replace project wide.
    "MagicDuck/grug-far.nvim",
    opts = {},
  },
  { -- Terminal Emulator helpers.
    "amirrezaask/nvim-terminal.lua",
    config = function()
      vim.keymap.set({ "n", "t" }, "<C-j>", require("nvim-terminal").toggle_floating)
    end,
  },
  { -- My simple statusline.
    "amirrezaask/nvim-statusline.lua",
    config = function()
      local statusline = require("statusline")
      local sections = statusline.sections
      statusline.setup {
        sections.ModeSection,
        " ",
        sections.GitBranchSection,
        sections.SeperatorSection,
        sections.FileTypeIcon(),
        "  ",
        sections.FileSection { shorten_style = "elipsis" },
        sections.ModifiedSection,
        sections.SeperatorSection,
        sections.FileTypeSection,
        "[",
        sections.LineSection,
        ":",
        sections.ColumnSection,
        "]",
      }
    end,
  },
})

o.wrap = true
o.breakindent = true
o.signcolumn = "yes"
o.swapfile = false
o.backup = false
o.undofile = true
o.splitbelow = true
o.splitright = true
o.showmode = false
o.timeoutlen = 300
o.updatetime = 250
o.clipboard = "unnamedplus"
o.ignorecase = true
o.smartcase = true
o.cursorline = true -- Highlight current line
o.guicursor = o.guicursor .. ",t:ver25"
o.laststatus = 3 -- Single Statusline for all windows
o.number = true -- Line numbers
o.termguicolors = true
o.winborder = "rounded"
o.inccommand = "split"
o.more = true
o.relativenumber = true
o.list = true
o.listchars = "tab:  ,trail:␣,eol:↲"
o.scrolloff = 10
-- o.statusline = "%F [%l:%c]"

keymap("n", "Y", "^v$y", { desc = "Copy whole line" })
keymap("t", "<esc>", [[<C-\><C-n>]])
keymap("i", "<C-c>", "<esc>")
keymap("n", "<C-d>", "<C-d>zz")
keymap("n", "<C-u>", "<C-u>zz")
keymap("n", "n", "nzz")
keymap("n", "N", "Nzz")
keymap("i", "jk", "<ESC>")
keymap("i", "kj", "<ESC>")
keymap("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
keymap("n", "j", "gj")
keymap("n", "k", "gk")
keymap("n", "{", "<cmd>cprev<CR>")
keymap("n", "}", "<cmd>cnext<CR>")
keymap("n", "<M-k>", ":bwipe!<CR>")
keymap("n", "<C-q>", function()
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

vim.diagnostic.config({ virtual_text = true })

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    vim.keymap.set("n", "[[", function()
      vim.diagnostic.jump({ count = -1 })
    end, { buffer = args.buf })
    keymap("n", "]]", function()
      vim.diagnostic.jump({ count = 1 })
    end, { buffer = args.buf })
    keymap("n", "C-]", vim.lsp.buf.definition, { buffer = args.buf })
    keymap("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
    keymap("n", "gD", vim.lsp.buf.declaration, { buffer = args.buf })
    keymap("n", "gr", vim.lsp.buf.references, { buffer = args.buf })
    keymap("n", "gi", vim.lsp.buf.implementation, { buffer = args.buf })
    keymap("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
    keymap("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
    keymap("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
    keymap({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, { buffer = args.buf })
    keymap("n", "<leader>l", vim.diagnostic.open_float, { buffer = args.buf })
    keymap("n", "<leader>q", vim.diagnostic.setloclist, { buffer = args.buf })
  end,
})

vim.cmd.colorscheme(vim.env.NVIM_COLORSCHEME or "tokyonight-moon")

function Transparent()
  vim.cmd [[
    hi Normal guibg=none
    hi NormalFloat guibg=none
    hi NormalNC guibg=none
    hi FloatBorder guibg=none
    hi SignColumn guibg=none
    hi LineNr guibg=none
  ]]
end

Transparent()

vim.api.nvim_create_user_command("Transparent", Transparent, {})

-- Programming languages setup
vim.api.nvim_create_autocmd("FileType", {
  pattern = "go",
  callback = function(args)
    vim.bo[args.buf].sw = 4
    vim.bo[args.buf].ts = 4
    vim.bo[args.buf].expandtab = false
    vim.bo[args.buf].shiftwidth = 4
    vim.lsp.start({
      cmd = { "gopls" },
      filetypes = { "go" },
      root_markers = { "go.mod", "go.sum", ".git" },
    })

    vim.keymap.set("n", "<C-enter>", function()
      require("terminal").toggle_right({ cmd = "go build ./..." })
    end, { buffer = args.buf })

    vim.keymap.set("n", "<M-enter>", function()
      require("terminal").get({ cmd = "go test ./..." })
    end, { buffer = args.buf })
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "lua",
  callback = function(args)
    vim.keymap.set("n", "<C-enter>", ":so %<CR>", { buffer = args.buf })
    vim.bo[args.buf].sw = 2
    vim.bo[args.buf].ts = 2
    vim.bo[args.buf].expandtab = true
    vim.bo[args.buf].shiftwidth = 2
    vim.lsp.start({
      cmd = { "lua-language-server" },
      filetypes = { "lua" },
      root_markers = { ".git" },
      settings = {
        Lua = {
          workspace = {
            userThirdParty = { os.getenv("HOME") .. ".local/share/LuaAddons" },
            checkThirdParty = "Apply",
          },
          diagnostics = {
            globals = { "vim" },
          },
        },
      },
    })
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "php",
  callback = function(args)
    vim.bo[args.buf].sw = 4
    vim.bo[args.buf].ts = 4
    vim.bo[args.buf].expandtab = false
    vim.bo[args.buf].shiftwidth = 4

    vim.diagnostic.config({ virtual_text = false })

    vim.lsp.start({
      cmd = { "intelephense", "--stdio" },
      filetypes = { "php" },
      root_markers = { "composer.json", ".git" },
    })
  end,
})
