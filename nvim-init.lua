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

vim.g.mapleader = " "
vim.g.maplocalleader = ","
vim.o.rtp = vim.o.rtp .. "," .. lazypath

require("lazy").setup({
  { "amirrezaask/nvim-gruvbuddy.lua" }, -- Colorscheme, inspired by great @tjdevries.
  {
    "rose-pine/neovim",
    name = "rose-pine",
    opts = {
      styles = {
        italic = false,
        transparency = true,
      },
    },
  }, -- Colorscheme.
  { "folke/tokyonight.nvim" }, -- Colorscheme.

  {
    "amirrezaask/nvim-statusline.lua",
    config = function()
      require("statusline").setup()
    end,
  },

  {
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
  }, -- LSP configurations.

  { "nvim-tree/nvim-web-devicons" }, -- Icons in terminal, nice.

  { "supermaven-inc/supermaven-nvim", opts = {} }, -- AI Apocalypse
  { "MagicDuck/grug-far.nvim", opts = {} }, -- Find/Replace project wide.
  {
    "stevearc/oil.nvim",
    config = function()
      require("oil").setup {}

      vim.keymap.set("n", "-", "<cmd>Oil<CR>")
    end,
  }, -- File manager done right.
  { "williamboman/mason.nvim", opts = {} }, -- Package manager for your system inside neovim.
  {
    "saghen/blink.cmp",
    tag = "v1.1.1",
    opts = {
      keymap = { preset = "enter" },
      cmdline = { enabled = false },
    },
  }, -- Blazingly fast autocomplete
  {
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

      vim.api.nvim_create_autocmd("BufWritePre", {
        pattern = { "*.lua", "*.go" },
        callback = function(args)
          require("conform").format({ bufnr = args.buf })
        end,
      })
    end,
  }, -- Autoformat/fixes

  -- Fuzzy Finders
  {
    "folke/snacks.nvim",
    config = function()
      require("snacks").setup {
        picker = { enabled = true },
        bigfile = { enabled = true },
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
      vim.keymap.set("n", "<leader>fs", function()
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
      vim.keymap.set("v", "??", SnacksPicker.grep_word, { desc = "Grep word under cursor" })
      vim.keymap.set("n", "<leader>o", SnacksPicker.lsp_symbols, { desc = "LSP Document Symbols" })
      vim.keymap.set("n", "<leader>O", SnacksPicker.lsp_workspace_symbols, { desc = "LSP Workspace Symbols" })
      vim.keymap.set("n", "<M-o>", SnacksPicker.lsp_workspace_symbols, { desc = "LSP Workspace Symbols" })
      vim.keymap.set("n", "<leader>fd", function()
        SnacksPicker.files({ cwd = "~/.dotfiles" })
      end, { desc = "Find Dotfiles" })

      vim.lsp.buf.definition = SnacksPicker.lsp_definitions
      vim.lsp.buf.implementation = SnacksPicker.lsp_implementations
      vim.lsp.buf.references = SnacksPicker.lsp_references
      vim.lsp.buf.type_definition = SnacksPicker.lsp_type_definitions
    end,
  }, -- Plugin collection but i use the picker only.

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

  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
    config = function()
      ---@diagnostic disable-next-line: missing-fields
      require("nvim-treesitter.configs").setup {
        ensure_installed = "all",
        highlight = { enable = true },
        auto_install = true,
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<C-space>",
            node_incremental = "<C-space>",
            scope_incremental = false,
            node_decremental = "<bs>",
          },
        },
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

  { "folke/lazydev.nvim", opts = { library = {} } }, -- Better neovim development support.
})

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
vim.o.list = true -- Show whitespace
vim.o.listchars = "tab:  ,trail:·,extends: ,precedes: ,eol:↲,conceal:┊,nbsp:␣"
vim.o.title = true
vim.o.titlestring = "nvim %M%F" -- Set title of the terminal.

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
vim.keymap.set(
  "n",
  "<CR>",
  function() -- Taken from https://github.com/tjdevries/config.nvim/blob/master/plugin/keymaps.lua#L16
    if vim.v.hlsearch == 1 then
      vim.cmd.nohl()
      return ""
    else
      ---@diagnostic disable-next-line: undefined-field
      return vim.keycode "<CR>"
    end
  end,
  { expr = true }
)
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
local function toggle_bottom_terminal() -- Toggle terminal at the bottom of the screen, why install a plugin for this ?
  -- We have a valid buffer showing the terminal
  if not vim.g.bottom_terminal_buffer or not vim.api.nvim_buf_is_valid(vim.g.bottom_terminal_buffer) then
    vim.g.bottom_terminal_buffer = vim.api.nvim_create_buf(false, true)
  end
  for _, win in ipairs(vim.api.nvim_list_wins()) do -- Toggle if a window showing terminal is open
    local win_buf = vim.api.nvim_win_get_buf(win)
    if win_buf == vim.g.bottom_terminal_buffer then
      vim.api.nvim_win_hide(win)
      return
    end
  end

  local win = vim.api.nvim_open_win(vim.g.bottom_terminal_buffer, true, {
    win = -1,
    split = "below",
    height = math.floor(vim.o.lines * 0.45),
    width = vim.o.columns,
  })
  vim.wo[win].winfixheight = true
  vim.wo[win].list = false
  vim.wo[win].scrolloff = 0
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false

  if vim.bo[vim.g.bottom_terminal_buffer].buftype ~= "terminal" then
    vim.cmd.term()
    vim.g.bottom_terminal_buffer = vim.api.nvim_get_current_buf()
  end

  vim.cmd.startinsert()
end
vim.keymap.set({ "n", "t" }, "<C-s>", toggle_bottom_terminal, { desc = "Toggle bottom terminal" })

vim.g.gruvbuddy_style = "dark"

vim.cmd.colorscheme("gruvbuddy")

-- FileType Settings
local function ftplugin(filetype, callback)
  local ft = vim.api.nvim_create_augroup("ftplugin_" .. filetype, {})
  vim.api.nvim_create_autocmd("FileType", {
    pattern = filetype,
    group = ft,
    callback = callback,
  })
end
ftplugin("go", function()
  vim.bo[0].sw = 4
  vim.bo[0].ts = 4
  vim.bo[0].expandtab = false
  vim.bo[0].shiftwidth = 4
end)

ftplugin("lua", function()
  vim.bo[0].sw = 2
  vim.bo[0].ts = 2
  vim.bo[0].expandtab = true
  vim.bo[0].shiftwidth = 2
end)

ftplugin("ocaml", function()
  vim.bo[0].sw = 2
  vim.bo[0].ts = 2
  vim.bo[0].expandtab = true
  vim.bo[0].shiftwidth = 2
end)

ftplugin("php", function()
  vim.bo[0].sw = 4
  vim.bo[0].ts = 4
  vim.bo[0].expandtab = true
  vim.bo[0].shiftwidth = 4
end)

ftplugin("sh", function()
  vim.bo[0].sw = 2
  vim.bo[0].ts = 2
  vim.bo[0].expandtab = true
  vim.bo[0].shiftwidth = 2
end)
