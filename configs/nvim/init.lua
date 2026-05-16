vim.g.colorscheme = os.getenv("NVIM_THEME") or "default"

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
vim.o.mouse = "a"

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
         hi! Normal guibg=none
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
    enabled = vim.g.colors,
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
  { "morhetz/gruvbox", enabled = vim.g.colors, config = function() vim.g.gruvbox_contrast_dark = "hard" end },
  { "folke/tokyonight.nvim", opts = { styles = { comments = { italic = false }, keywords = { italic = false } } } },
  { "catppuccin/nvim", enabled = vim.g.colors, name = "catppuccin" },
  { "rose-pine/neovim", enabled = vim.g.colors, name = "rose-pine" },
  -- Fuzzy Finder
  {
    "https://github.com/ibhagwan/fzf-lua",
    dependencies = { { "nvim-tree/nvim-web-devicons" } },
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
      vim.keymap.set("n", "<leader>l", FzfLua.diagnostics_document, { desc = "Diagnostics Document" })
      vim.keymap.set("n", "<leader>L", FzfLua.diagnostics_workspace, { desc = "Diagnostics Workspace" })
      vim.keymap.set("n", "<leader>;", FzfLua.commands, { desc = "Commands" })
      vim.keymap.set({ "n", "v", "x" }, "<leader>J", FzfLua.grep_cword, { desc = "Grep Word" })

      FzfLua.register_ui_select()
      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function(args)
          local opts = {
            buffer = args.buf,
          }
          vim.keymap.set({ "n" }, "gd", FzfLua.lsp_definitions, opts)
          vim.keymap.set({ "n" }, "grr", FzfLua.lsp_references, opts)
          vim.keymap.set({ "n" }, "gri", FzfLua.lsp_implementations, opts)
        end,
      })
    end,
  },

  {
    "esmuellert/codediff.nvim",
    opts = {
      -- Highlight configuration
      highlights = {
        -- Line-level: accepts highlight group names or hex colors (e.g., "#2ea043")
        line_insert = "DiffAdd", -- Line-level insertions
        line_delete = "DiffDelete", -- Line-level deletions

        -- Character-level: accepts highlight group names or hex colors
        -- If specified, these override char_brightness calculation
        char_insert = nil, -- Character-level insertions (nil = auto-derive)
        char_delete = nil, -- Character-level deletions (nil = auto-derive)

        -- Brightness multiplier (only used when char_insert/char_delete are nil)
        -- nil = auto-detect based on background (1.4 for dark, 0.92 for light)
        char_brightness = nil, -- Auto-adjust based on your colorscheme

        -- Conflict sign highlights (for merge conflict views)
        -- Accepts highlight group names or hex colors (e.g., "#f0883e")
        -- nil = use default fallback chain
        conflict_sign = nil, -- Unresolved: DiagnosticSignWarn -> #f0883e
        conflict_sign_resolved = nil, -- Resolved: Comment -> #6e7681
        conflict_sign_accepted = nil, -- Accepted: GitSignsAdd -> DiagnosticSignOk -> #3fb950
        conflict_sign_rejected = nil, -- Rejected: GitSignsDelete -> DiagnosticSignError -> #f85149
      },

      -- Diff view behavior
      diff = {
        layout = "side-by-side", -- Diff layout: "side-by-side" (two panes) or "inline" (single pane with virtual lines)
        disable_inlay_hints = true, -- Disable inlay hints in diff windows for cleaner view
        max_computation_time_ms = 5000, -- Maximum time for diff computation (VSCode default)
        ignore_trim_whitespace = false, -- Ignore leading/trailing whitespace changes (like diffopt+=iwhite)
        hide_merge_artifacts = false, -- Hide merge tool temp files (*.orig, *.BACKUP.*, *.BASE.*, *.LOCAL.*, *.REMOTE.*)
        original_position = "left", -- Position of original (old) content: "left" or "right"
        conflict_ours_position = "right", -- Position of ours (:2) in conflict view: "left" or "right"
        conflict_result_position = "bottom", -- "bottom" (default): result below diff panes or "center": result between diff panes (three columns)
        conflict_result_height = 30, -- Height of result pane in bottom layout (% of total height)
        conflict_result_width_ratio = { 1, 1, 1 }, -- Width ratio for center layout panes {left, center, right} (e.g., {1, 2, 1} for wider result)
        cycle_next_hunk = true, -- Wrap around when navigating hunks (]c/[c): false to stop at first/last
        cycle_next_file = true, -- Wrap around when navigating files (]f/[f): false to stop at first/last
        jump_to_first_change = true, -- Auto-scroll to first change when opening a diff: false to stay at same line
        highlight_priority = 100, -- Priority for line-level diff highlights (increase to override LSP highlights)
        compute_moves = false, -- Detect moved code blocks (opt-in, matches VSCode experimental.showMoves)
      },

      -- Explorer panel configuration
      explorer = {
        position = "left", -- "left" or "bottom"
        width = 40, -- Width when position is "left" (columns)
        height = 15, -- Height when position is "bottom" (lines)
        indent_markers = true, -- Show indent markers in tree view (│, ├, └)
        initial_focus = "explorer", -- Initial focus: "explorer", "original", or "modified"
        icons = {
          folder_closed = "", -- Nerd Font folder icon (customize as needed)
          folder_open = "", -- Nerd Font folder-open icon
        },
        view_mode = "list", -- "list" or "tree"
        flatten_dirs = true, -- Flatten single-child directory chains in tree view
        file_filter = {
          ignore = { ".git/**", ".jj/**" }, -- Glob patterns to hide (e.g., {"*.lock", "dist/*"})
        },
        focus_on_select = false, -- Jump to modified pane after selecting a file (default: stay in explorer)
        status_right_margin = 1, -- Trailing cells between status symbol (M/A/D) and right edge; increase if Nerd Font icons clip it
        visible_groups = { -- Which groups to show (can be toggled at runtime)
          staged = true,
          unstaged = true,
          conflicts = true,
        },
      },

      -- History panel configuration (for :CodeDiff history)
      history = {
        position = "bottom", -- "left" or "bottom" (default: bottom)
        width = 40, -- Width when position is "left" (columns)
        height = 15, -- Height when position is "bottom" (lines)
        initial_focus = "history", -- Initial focus: "history", "original", or "modified"
        view_mode = "list", -- "list" or "tree" for files under commits
      },

      -- Keymaps in diff view
      keymaps = {
        view = {
          quit = "q", -- Close diff tab
          toggle_explorer = "<leader>b", -- Toggle explorer visibility (explorer mode only)
          focus_explorer = "<leader>e", -- Focus explorer panel (explorer mode only)
          next_hunk = "]c", -- Jump to next change
          prev_hunk = "[c", -- Jump to previous change
          next_file = "]f", -- Next file in explorer/history mode
          prev_file = "[f", -- Previous file in explorer/history mode
          diff_get = "do", -- Get change from other buffer (like vimdiff)
          diff_put = "dp", -- Put change to other buffer (like vimdiff)
          open_in_prev_tab = "gf", -- Open current buffer in previous tab (or create one before)
          close_on_open_in_prev_tab = false, -- Close codediff tab after gf opens file in previous tab
          toggle_stage = "-", -- Stage/unstage current file (works in explorer and diff buffers)
          stage_hunk = "<leader>hs", -- Stage hunk under cursor to git index
          unstage_hunk = "<leader>hu", -- Unstage hunk under cursor from git index
          discard_hunk = "<leader>hr", -- Discard hunk under cursor (working tree only)
          hunk_textobject = "ih", -- Textobject for hunk (vih to select, yih to yank, etc.)
          show_help = "g?", -- Show floating window with available keymaps
          align_move = "gm", -- Temporarily align moved code blocks across panes
          toggle_layout = "t", -- Toggle between side-by-side and inline layout
        },
        explorer = {
          select = "<CR>", -- Open diff for selected file
          hover = "K", -- Show file diff preview
          refresh = "R", -- Refresh git status
          toggle_view_mode = "i", -- Toggle between 'list' and 'tree' views
          stage_all = "S", -- Stage all files
          unstage_all = "U", -- Unstage all files
          restore = "X", -- Discard changes (restore file)
          toggle_changes = "gu", -- Toggle Changes (unstaged) group visibility
          toggle_staged = "gs", -- Toggle Staged Changes group visibility
          -- Fold keymaps (Vim-style)
          fold_open = "zo", -- Open fold (expand current node)
          fold_open_recursive = "zO", -- Open fold recursively (expand all descendants)
          fold_close = "zc", -- Close fold (collapse current node)
          fold_close_recursive = "zC", -- Close fold recursively (collapse all descendants)
          fold_toggle = "za", -- Toggle fold (expand/collapse current node)
          fold_toggle_recursive = "zA", -- Toggle fold recursively
          fold_open_all = "zR", -- Open all folds in tree
          fold_close_all = "zM", -- Close all folds in tree
        },
        history = {
          select = "<CR>", -- Select commit/file or toggle expand
          toggle_view_mode = "i", -- Toggle between 'list' and 'tree' views
          refresh = "R", -- Refresh history (re-fetch commits)
          -- Fold keymaps (Vim-style, apply to directory nodes only)
          fold_open = "zo", -- Open fold (expand current node)
          fold_open_recursive = "zO", -- Open fold recursively (expand all descendants)
          fold_close = "zc", -- Close fold (collapse current node)
          fold_close_recursive = "zC", -- Close fold recursively (collapse all descendants)
          fold_toggle = "za", -- Toggle fold (expand/collapse current node)
          fold_toggle_recursive = "zA", -- Toggle fold recursively
          fold_open_all = "zR", -- Open all folds in tree
          fold_close_all = "zM", -- Close all folds in tree
        },
        conflict = {
          accept_incoming = "<leader>ct", -- Accept incoming (theirs/left) change
          accept_current = "<leader>co", -- Accept current (ours/right) change
          accept_both = "<leader>cb", -- Accept both changes (incoming first)
          discard = "<leader>cx", -- Discard both, keep base
          -- Accept all (whole file) - uppercase versions
          accept_all_incoming = "<leader>cT", -- Accept ALL incoming changes
          accept_all_current = "<leader>cO", -- Accept ALL current changes
          accept_all_both = "<leader>cB", -- Accept ALL both changes
          discard_all = "<leader>cX", -- Discard ALL, reset to base
          next_conflict = "]x", -- Jump to next conflict
          prev_conflict = "[x", -- Jump to previous conflict
          diffget_incoming = "2do", -- Get hunk from incoming (left/theirs) buffer
          diffget_current = "3do", -- Get hunk from current (right/ours) buffer
        },
      },
    },
    config = function(_, opts)
      require("codediff").setup(opts)
      vim.keymap.set({ "n" }, "<leader>gd", ":CodeDiff<CR>")
    end,
  },

  { "lewis6991/gitsigns.nvim", opts = {} },

  { "tpope/vim-sleuth" },

  {
    "neovim/nvim-lspconfig",
    dependencies = {
      { "mason-org/mason.nvim", opts = {} },
      { "mason-org/mason-lspconfig.nvim", opts = { ensure_installed = { "lua_ls", "gopls", "ts_ls" } } },
    },
    config = function()
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
        javascript = { "prettierd" },
        typescript = { "prettierd" },
        javascriptreact = { "prettierd" },
        typescriptreact = { "prettierd" },
        json = { "prettierd" },
        jsonc = { "prettierd" },
        yaml = { "prettierd" },
        markdown = { "prettierd" },
        html = { "prettierd" },
        css = { "prettierd" },
        scss = { "prettierd" },
      },
      format_on_save = {
        timeout_ms = 500,
        lsp_fallback = false,
      },
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
        typescript = { "eslint_d" },
        typescriptreact = { "eslint_d" },
        go = { "golangcilint" },
      }
      vim.api.nvim_create_autocmd({ "BufWritePost", "BufReadPost", "InsertLeave" }, {
        callback = function() pcall(require("lint").try_lint) end,
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
