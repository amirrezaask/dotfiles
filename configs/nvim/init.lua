vim.g.colorscheme = os.getenv("NVIM_THEME") or "default"
vim.g.fuzzy_finder = os.getenv("NVIM_FUZZY_FINDER") or "fzf"
vim.g.noice = os.getenv("NVIM_NOICE") == "true" or false
vim.g.lualine = os.getenv("NVIM_LUALINE") == "true" or false

-- -----------------------------------------------------------------------------
-- Core editor behavior
-- -----------------------------------------------------------------------------

vim.o.undofile = true -- Persist undo history across editor restarts.
vim.o.swapfile = false -- Avoid creating swap files next to edited files.
vim.o.number = true -- Show absolute line numbers.
vim.o.relativenumber = true -- Show relative line numbers for faster motions.
vim.o.signcolumn = "yes" -- Keep the sign column visible to avoid text shifting.
vim.o.cursorline = true -- Highlight the line under the cursor.
vim.o.scrolloff = 10 -- Keep five lines of context above/below the cursor.
vim.o.linebreak = true -- Wrap long lines at word boundaries instead of mid-word.
vim.o.winborder = "rounded" -- Use rounded borders for floating windows.
vim.o.laststatus = 3 -- Use one global statusline instead of one per window.
vim.o.showmode = false -- Don't show mode in command line (shown in statusline)
vim.o.showcmd = false -- Don't show partial command in command line
vim.o.title = true
vim.o.titlestring = "%{fnamemodify(getcwd(), ':~')}"
vim.o.shortmess = vim.o.shortmess .. "I"

-- ============================================================================
-- Disable Built-in Plugins
-- ============================================================================
vim.g.loaded_netrw = 1 -- Disable netrw file explorer (using a different file explorer)
vim.g.loaded_netrwPlugin = 1 -- Disable netrw plugin component

-- ============================================================================
-- Disable Providers (silence health check warnings)
-- ============================================================================
vim.g.loaded_node_provider = 0 -- Disable Node.js provider
vim.g.loaded_perl_provider = 0 -- Disable Perl provider
vim.g.loaded_python3_provider = 0 -- Disable Python 3 provider
vim.g.loaded_ruby_provider = 0 -- Disable Ruby provider

-- -----------------------------------------------------------------------------
-- Folding
-- -----------------------------------------------------------------------------

vim.o.foldmethod = "expr" -- Compute folds from an expression instead of markers/indent.
vim.o.foldexpr = "v:lua.vim.treesitter.foldexpr()" -- Let Tree-sitter provide semantic folds.
vim.o.foldcolumn = "0" -- Hide the fold column in the gutter.
vim.o.foldtext = "" -- Use the default line text for folded regions.
vim.o.foldlevel = 99 -- Keep almost all folds open after changing buffers.
vim.o.foldlevelstart = 99 -- Start with moderately nested folds open when reading files.

-- -----------------------------------------------------------------------------
-- Indentation & Tabs
-- -----------------------------------------------------------------------------
vim.o.shiftround = true
vim.o.shiftwidth = 2
vim.o.tabstop = 2
vim.o.expandtab = true

-- -----------------------------------------------------------------------------
-- Searching
-- -----------------------------------------------------------------------------
vim.o.ignorecase = true -- Search case-insensitively by default.
vim.o.smartcase = true -- Switch to case-sensitive search when the pattern has capitals.
vim.o.inccommand = "split" -- Preview substitutions in a live split window.

vim.o.formatoptions = "jcql" -- Control automatic comment/text formatting behavior.
vim.o.completeopt = "menuone,noselect,noinsert,fuzzy" -- Show completion menu without auto-inserting.

-- -----------------------------------------------------------------------------
-- Window[s]
-- -----------------------------------------------------------------------------
vim.o.splitbelow = true -- Open horizontal splits below the current window.
vim.o.splitright = true -- Open vertical splits to the right of the current window.
vim.o.splitkeep = "topline" -- Preserve the top visible line when opening splits.

vim.o.clipboard = "unnamedplus" -- Sync yank/delete/put with the system clipboard.

vim.opt.wildoptions:append("fuzzy") -- Enable fuzzy matching for command-line completion.

vim.diagnostic.config { virtual_text = false } -- Prefer diagnostic floats over inline virtual text.

vim.o.pumheight = 10 -- Limit completion popup height.
vim.o.pumblend = 10 -- Make the completion popup slightly transparent.

-- EXPERIMENTAL: Enable Neovim's new core UI layer when it exists.
local ok, ui2 = pcall(require, "vim._core.ui2")
if ok then ui2.enable { enable = true } end

-- -----------------------------------------------------------------------------
-- Autocommands
-- -----------------------------------------------------------------------------

vim.api.nvim_create_autocmd("TextYankPost", {
  -- Flash the yanked region so it is obvious what was copied.
  callback = function() vim.hl.on_yank { higroup = "Visual", timeout = 150 } end,
})

vim.api.nvim_create_autocmd("BufEnter", {
  -- Prompt buffers do their own input handling, so autocomplete gets in the way.
  callback = function(args)
    if vim.bo[args.buf].buftype == "prompt" then vim.bo[args.buf].autocomplete = false end
  end,
})

vim.api.nvim_create_autocmd("VimResized", {
  command = "wincmd =",
}) -- Equalize split sizes after terminal/window resize.

vim.api.nvim_create_autocmd("BufReadPost", {
  -- Restore the cursor to the last saved position when reopening a normal file.
  callback = function(args)
    local mark = vim.api.nvim_buf_get_mark(args.buf, '"')
    local line_count = vim.api.nvim_buf_line_count(args.buf)
    if mark[1] > 0 and mark[1] <= line_count then
      vim.api.nvim_win_set_cursor(0, mark)
      vim.schedule(function()
        vim.cmd("normal! zz") -- Center the restored cursor after the window settles.
      end)
    end
  end,
})

-- -----------------------------------------------------------------------------
-- Keymaps
-- -----------------------------------------------------------------------------

vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.keymap.set({ "n", "i" }, "<M-d>", "<cmd>vnew<CR>")
vim.keymap.set({ "n", "i" }, "<M-D>", "<cmd>new<CR>")

vim.keymap.set("i", "jk", "<esc>") -- Leave insert mode without reaching for Escape.
vim.keymap.set("i", "kj", "<esc>") -- Alternate insert-mode Escape chord.
vim.keymap.set("i", "<C-c>", "<esc>") -- Make Ctrl-C behave like Escape in insert mode.

vim.keymap.set({ "n", "x", "o" }, "H", "^", { desc = "Start of Line" })
vim.keymap.set({ "n", "x", "o" }, "L", "g_", { desc = "End of Line" })

-- Better indenting (stay in visual mode)
vim.keymap.set("v", "<", "<gv", { desc = "Indent Left" })
vim.keymap.set("v", ">", ">gv", { desc = "Indent Right" })

vim.keymap.set("v", "p", '"_dP', { desc = "Paste (no yank)" }) -- Paste over selection without yanking

vim.keymap.set("n", "<C-d>", "<C-d>zz") -- Half-page down and recenter.
vim.keymap.set("n", "<C-u>", "<C-u>zz") -- Half-page up and recenter.

vim.keymap.set("n", "n", "nzz") -- Next search result and recenter.
vim.keymap.set("n", "N", "Nzz") -- Previous search result and recenter.

vim.keymap.set("n", "j", "gj") -- Move by visual lines when text wraps.
vim.keymap.set("n", "k", "gk") -- Move by visual lines when text wraps.

vim.keymap.set("n", "<leader>i", ":edit $MYVIMRC<CR>", { desc = "Edit Configuration" })
vim.keymap.set("n", "<C-q>", function()
  if vim.fn.getqflist({ winid = 0 }).winid ~= 0 then
    vim.cmd.cclose()
  else
    vim.cmd.copen()
  end
end, {
  desc = "Toggle quickfix list",
})

vim.keymap.set("i", "<C-Space>", "<C-x><C-o>", { desc = "Trigger LSP completion" })
vim.keymap.set("n", "<CR>", function()
  -- Pressing Enter clears an active search highlight; otherwise it behaves normally.
  if vim.v.hlsearch == 1 then
    vim.cmd.nohl()
    return ""
  else
    return vim.keycode("<CR>")
  end
end, {
  expr = true,
})

vim.g.dotfiles_location = "~/dev/dotfiles"

vim.api.nvim_create_autocmd("ColorScheme", {
  pattern = "default",
  callback = function(args)
    if args.match == "default" then vim.cmd([[ 
        hi! Normal guibg=#000000 
        hi! link SnacksPickerDir Normal
]]) end
  end,
})

-- -----------------------------------------------------------------------------
-- Plugins
-- ---------------------------------------------------------------------------

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  vim.fn.system {
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  { "vague-theme/vague.nvim", opts = { bold = false, italic = false } },
  { "scottmckendry/cyberdream.nvim", opts = {} },
  { "navarasu/onedark.nvim", opts = { style = "darker" } },
  {
    "sainnhe/everforest",
    config = function()
      vim.g.everforest_background = "hard"
      vim.g.everforest_enable_italic = 0
      vim.api.nvim_create_autocmd("ColorScheme", {
        pattern = "everforest",
        callback = function()
          if vim.o.background == "dark" then
            vim.cmd([[
            hi! Normal guibg=#1e2326 guifg=#ffffff
            hi! NormalFloat guibg=#1e2326 guifg=#ffffff
            hi! Terminal guibg=#1e2326 guifg=#ffffff
          ]])
          end
        end,
      })
    end,
  },
  {
    "ellisonleao/gruvbox.nvim",
    opts = {
      undercurl = false,
      underline = false,
      bold = false,
      italic = {
        strings = false,
        emphasis = false,
        comments = false,
        operators = false,
        folds = false,
      },
      contrast = "hard",
    },
  },

  { "folke/tokyonight.nvim", opts = { styles = { comments = { italic = false }, keywords = { italic = false } } } },
  { "catppuccin/nvim", name = "catppuccin" },
  { "rose-pine/neovim", name = "rose-pine" },
  -- Fuzzy Finder
  {
    "https://github.com/ibhagwan/fzf-lua",
    enabled = vim.g.fuzzy_finder == "fzf",
    opts = {},
    config = function(_, opts)
      require("fzf-lua").setup(opts)
      FzfLua = require("fzf-lua")
      FzfLua.setup { "fzf-vim" }
      vim.keymap.set("n", "<leader><leader>", FzfLua.files, { desc = "Find Files" })
      vim.keymap.set("n", "<leader>i", function() FzfLua.files { cwd = "~/dev/dotfiles" } end, { desc = "Find Configuration" })
      vim.keymap.set("n", "<leader>pf", FzfLua.git_files, { desc = "Git Files" })
      vim.keymap.set("n", "<leader>k", FzfLua.buffers, { desc = "Buffers" })
      vim.keymap.set("n", "<leader>j", FzfLua.live_grep, { desc = "Grep" })
      vim.keymap.set("n", "<leader>h", FzfLua.helptags, { desc = "Help Tags" })
      vim.keymap.set("n", "<leader>;", FzfLua.commands, { desc = "Commands" })
      vim.keymap.set({ "n", "v", "x" }, "<leader>J", FzfLua.grep_cword, { desc = "Grep Word" })

      FzfLua.register_ui_select()
    end,
  },

  {
    "nvim-lualine/lualine.nvim",
    enabled = vim.g.lualine,
    opts = {
      sections = {
        lualine_a = {
          {
            "filename",
            path = 1,
          },
        },
      },
    },
  },

  { "lewis6991/gitsigns.nvim", opts = {} },
  { "nvim-tree/nvim-web-devicons" },

  { "tpope/vim-sleuth" },

  {
    "folke/snacks.nvim",
    opts = {
      bigfile = { enabled = true },
      indent = { enabled = true },
      input = { enabled = true },
      picker = { enabled = vim.g.fuzzy_finder == "snacks" },
      quickfile = { enabled = true },
      statuscolumn = { enabled = true },
    },
    config = function(_, opts) require("snacks").setup(opts) end,
    keys = {
      -- ════════════════════════════════════════════════════════════════════
      -- Top-level (most used - quick access)
      -- ════════════════════════════════════════════════════════════════════
      { "<leader><leader>", function() Snacks.picker.files() end, desc = "Find Files" },
      { "<leader>j", function() Snacks.picker.grep() end, desc = "Grep" },
      { "<leader>i", function() Snacks.picker.files { cwd = vim.g.dotfiles_location } end, desc = "Grep" },
      { "<leader>J", function() Snacks.picker.grep_word() end, desc = "Grep" },
      { "<leader>k", function() Snacks.picker.buffers() end, desc = "Buffers" },

      -- ════════════════════════════════════════════════════════════════════
      -- <leader>d = Diagnostics
      -- ════════════════════════════════════════════════════════════════════
      { "<leader>dd", function() Snacks.picker.diagnostics() end, desc = "Workspace Diagnostics" },
      { "<leader>db", function() Snacks.picker.diagnostics_buffer() end, desc = "Buffer Diagnostics" },
      { "<leader>dq", function() Snacks.picker.qflist() end, desc = "Quickfix List" },
      { "<leader>dl", function() Snacks.picker.loclist() end, desc = "Location List" },

      -- ════════════════════════════════════════════════════════════════════
      -- <leader>f = Files
      -- ════════════════════════════════════════════════════════════════════
      { "<leader>ff", function() Snacks.picker.files() end, desc = "Find Files" },
      { "<leader>fr", function() Snacks.picker.recent() end, desc = "Recent Files" },
      { "<leader>fg", function() Snacks.picker.git_files() end, desc = "Git Files" },
      { "<leader>fp", function() Snacks.picker.projects() end, desc = "Projects" },
      { "<leader>fR", function() Snacks.rename.rename_file() end, desc = "Rename File" },

      -- ════════════════════════════════════════════════════════════════════
      -- <leader>g = Git
      -- ════════════════════════════════════════════════════════════════════
      { "<leader>gl", function() Snacks.picker.git_log() end, desc = "Log" },
      { "<leader>gL", function() Snacks.picker.git_log_line() end, desc = "Log (line)" },
      { "<leader>gf", function() Snacks.picker.git_log_file() end, desc = "Log (file)" },
      { "<leader>gs", function() Snacks.picker.git_status() end, desc = "Status" },
      { "<leader>gd", function() Snacks.picker.git_diff() end, desc = "Diff (picker)" },
      { "<leader>gc", function() Snacks.picker.git_branches() end, desc = "Checkout Branch" },

      -- ════════════════════════════════════════════════════════════════════
      -- <leader>s = Search
      -- ════════════════════════════════════════════════════════════════════
      { "<leader>sg", function() Snacks.picker.grep() end, desc = "Grep" },
      { "<leader>sw", function() Snacks.picker.grep_word() end, desc = "Word", mode = { "n", "x" } },
      { "<leader>sb", function() Snacks.picker.lines() end, desc = "Buffer Lines" },
      { "<leader>sB", function() Snacks.picker.grep_buffers() end, desc = "Grep Buffers" },
      { "<leader>sh", function() Snacks.picker.help() end, desc = "Help" },
      { "<leader>sm", function() Snacks.picker.marks() end, desc = "Marks" },
      { "<leader>sj", function() Snacks.picker.jumps() end, desc = "Jumps" },
      { "<leader>sk", function() Snacks.picker.keymaps() end, desc = "Keymaps" },
      { "<leader>sc", function() Snacks.picker.commands() end, desc = "Commands" },
      { "<leader>s:", function() Snacks.picker.command_history() end, desc = "Command History" },
      { "<leader>s/", function() Snacks.picker.search_history() end, desc = "Search History" },
      { "<leader>sr", function() Snacks.picker.registers() end, desc = "Registers" },
      { "<leader>sR", function() Snacks.picker.resume() end, desc = "Resume Last" },
      { "<leader>su", function() Snacks.picker.undo() end, desc = "Undo History" },
      { "<leader>sM", function() Snacks.picker.man() end, desc = "Man Pages" },
      { "<leader>si", function() Snacks.picker.icons() end, desc = "Icons" },

      -- ════════════════════════════════════════════════════════════════════
      -- LSP
      -- ════════════════════════════════════════════════════════════════════
      { "gd", function() Snacks.picker.lsp_definitions() end, desc = "[g]oto [d]efinition" },
      { "grr", function() Snacks.picker.lsp_references() end, desc = "[g]oto [r]eferences" },
      { "gri", function() Snacks.picker.lsp_implementations() end, desc = "[g]oto [i]mplmentations" },
      { "gO", function() Snacks.picker.lsp_symbols() end, desc = "[g]oto symbol" },
      { "<leader>o", function() Snacks.picker.lsp_symbols() end, desc = "[g]oto symbol" },
      { "<leader>O", function() Snacks.picker.lsp_workspace_symbols() end, desc = "[g]oto workspace symbol" },
    },
  },

  {
    "folke/noice.nvim",
    enabled = vim.g.noice,
    dependencies = { { "MunifTanjim/nui.nvim" } },
    opts = {
      lsp = {
        progress = { enabled = false },
        override = {
          ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
          ["vim.lsp.util.stylize_markdown"] = true,
          ["cmp.entry.get_documentation"] = true,
        },
      },
      popupmenu = { enabled = false },
      presets = {
        bottom_search = true,
        command_palette = true,
        long_message_to_split = true,
      },
    },
  },

  { "mason-org/mason.nvim", opts = {}, lazy = false },
  {
    "mason-org/mason-lspconfig.nvim",
    opts = { ensure_installed = { "lua_ls", "gopls", "ts_ls" } },
  },
  {
    "neovim/nvim-lspconfig",
    config = function()
      require("mason-lspconfig").setup { ensure_installed = { "lua_ls", "gopls", "ts_ls" } }

      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function(args)
          local opts = {
            buffer = args.buf,
            expr = true,
            replace_keycodes = false,
          }
          vim.keymap.set("i", "<Tab>", function()
            if vim.fn.pumvisible() == 1 then
              return vim.keycode("<C-y>")
            else
              return vim.keycode("<Tab>")
            end
          end, opts)

          vim.keymap.set("i", "<CR>", function()
            if vim.fn.pumvisible() == 1 then
              return vim.keycode("<C-y>")
            else
              return vim.keycode("<CR>")
            end
          end, opts)

          vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf, desc = "Open Floating Diagnostic" })
        end,
      })

      vim.lsp.config("lua_ls", {
        settings = {
          Lua = {
            diagnostics = { globals = { "vim" } },
            workspace = { library = vim.api.nvim_get_runtime_file("", true) },
          },
        },
      })
    end,
  },
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        php = nil,
        go = { "goimports" },
        lua = { "stylua" },
        javascript = { "prettier" },
        typescript = { "prettier" },
        javascriptreact = { "prettier" },
        typescriptreact = { "prettier" },
        json = { "prettier" },
        jsonc = { "prettier" },
        yaml = { "prettier" },
        markdown = { "prettier" },
        html = { "prettier" },
        css = { "prettier" },
        scss = { "prettier" },
      },
      format_on_save = {
        timeout_ms = 500,
        lsp_fallback = false,
      },
    },
  },
  {
    "folke/which-key.nvim",
    opts = {
      preset = "helix",
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      vim.api.nvim_create_autocmd("FileType", {
        callback = function(args) pcall(vim.treesitter.start, args.buf) end,
      })
      require("nvim-treesitter").install {
        "bash",
        "c",
        "cpp",
        "fish",
        "gitcommit",
        "go",
        "graphql",
        "html",
        "hyprlang",
        "java",
        "javascript",
        "json",
        "json5",
        "lua",
        "markdown",
        "markdown_inline",
        "python",
        "query",
        "rasi",
        "regex",
        "rust",
        "scss",
        "toml",
        "tsx",
        "typescript",
        "vim",
        "vimdoc",
        "yaml",
      }
    end,
  },
  {
    "mfussenegger/nvim-lint",
    config = function()
      require("lint").linters_by_ft = {
        typescript = { "eslint" },
        typescriptreact = { "eslint" },
      }
      vim.api.nvim_create_autocmd({ "BufWritePost", "TextChanged", "BufEnter" }, {
        callback = function() require("lint").try_lint() end,
      })
    end,
  },
  {
    "saghen/blink.cmp",
    version = "1.10.2",
    opts = {
      cmdline = {
        enabled = true,
        keymap = {
          preset = "cmdline",
          ["<Right>"] = false,
          ["<Left>"] = false,
        },
        completion = {
          list = { selection = { preselect = false } },
          menu = {
            auto_show = function(_) return vim.fn.getcmdtype() == ":" end,
          },
          ghost_text = { enabled = true },
        },
      },
      keymap = {
        preset = "super-tab",
        ["<C-y>"] = { "select_and_accept" },
        ["<enter>"] = { "select_and_accept", "fallback" },
        ["<tab>"] = { "select_and_accept", "fallback" },
      },
      completion = {
        accept = { auto_brackets = { enabled = true } },
        menu = { border = "none", draw = { treesitter = { "lsp" } } },
        documentation = { auto_show = true, auto_show_delay_ms = 200 },
        ghost_text = { enabled = true },
      },
    },
  },
}, {
  change_detection = {
    enabled = true,
    notify = false,
  },
})

vim.cmd.colorscheme(vim.g.colorscheme)
