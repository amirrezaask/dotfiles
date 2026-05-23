local start_time = vim.uv.hrtime()

-- -----------------------------------------------------------------------------
-- Core editor behavior
-- -----------------------------------------------------------------------------

vim.o.undofile = true -- Persist undo history across editor restarts.
vim.o.swapfile = false -- Avoid creating swap files next to edited files.
vim.o.number = true -- Show absolute line numbers.
vim.o.relativenumber = true -- Show relative line numbers for faster motions.
vim.o.signcolumn = "yes" -- Keep the sign column visible to avoid text shifting.
vim.o.cursorline = false -- Highlight the line under the cursor.
vim.o.scrolloff = 10 -- Keep five lines of context above/below the cursor.
vim.o.linebreak = true -- Wrap long lines at word boundaries instead of mid-word.
vim.o.winborder = "rounded" -- Use rounded borders for floating windows.
vim.o.laststatus = 3 -- Use one global statusline instead of one per window.
vim.o.showcmd = false -- Don't show partial command in command line
vim.o.title = true -- Control terminal title.
vim.o.titlestring = "%{fnamemodify(getcwd(), ':~')}" -- Terminal title will always be the cwd.
vim.o.shortmess = vim.o.shortmess .. "I" -- No Intro screen
vim.o.mouse = "a" -- Support Mouse in all modes
vim.o.autoread = true -- Auto refresh file state from the disk.
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter", "CursorHold" }, {
  callback = function() vim.cmd("checktime") end,
})

-- vim.o.guicursor = "" -- Don't change cursor shape when switching modes ( distracting ).

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

vim.diagnostic.config {
  virtual_text = { prefix = "●" },
  float = { border = "rounded" },
  signs = true,
  update_in_insert = false,
}

vim.o.updatetime = 300 -- ms before CursorHold fires (used for lint, etc.)
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

vim.api.nvim_create_user_command("Reload", function(_, _, _) vim.cmd.source("$MYVIMRC") end, {})

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
vim.keymap.set("n", "<leader>R", ":source $MYVIMRC<CR>", { desc = "Reload Configuration" })
vim.keymap.set("n", "<leader>t", ":edit ~/TODO.md<CR>", { desc = "Edit TODO.md" })
vim.keymap.set("n", "<C-q>", function()
  if vim.fn.getqflist({ winid = 0 }).winid ~= 0 then
    vim.cmd.cclose()
  else
    vim.cmd.copen()
  end
end, {
  desc = "Toggle quickfix list",
})
vim.keymap.set({ "n" }, "<leader>q", function() vim.diagnostic.setloclist { open = true } end)

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

vim.pack.add {
  "https://github.com/vague-theme/vague.nvim",
  "https://github.com/sainnhe/everforest",
  "https://github.com/morhetz/gruvbox",
  { src = "https://github.com/catppuccin/nvim", name = "catppuccin" },
  { src = "https://github.com/rose-pine/neovim", name = "rose-pine" },
  "https://github.com/scottmckendry/cyberdream.nvim",
  "https://github.com/navarasu/onedark.nvim",
  "https://github.com/folke/tokyonight.nvim",

  "https://github.com/ibhagwan/fzf-lua",
  "https://github.com/tpope/vim-sleuth",
  "https://github.com/folke/which-key.nvim",

  "https://github.com/neovim/nvim-lspconfig",
  "https://github.com/mason-org/mason.nvim",
  "https://github.com/mason-org/mason-lspconfig.nvim",
  "https://github.com/stevearc/conform.nvim",
  "https://github.com/mfussenegger/nvim-lint",
  "https://github.com/saghen/blink.cmp",
  "https://github.com/nvim-treesitter/nvim-treesitter",

  -- Git
  "https://github.com/NeogitOrg/neogit",
  "https://github.com/m00qek/baleia.nvim",
  "https://github.com/lewis6991/gitsigns.nvim",
  "https://github.com/sindrets/diffview.nvim",
}

-- ============================================================================
-- Plugin configurations
-- ============================================================================

-- Colorschemes
require("vague").setup { bold = false, italic = false }
require("onedark").setup { style = "darker" }
require("tokyonight").setup {
  styles = { comments = { italic = false }, keywords = { italic = false } },
}

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
vim.api.nvim_create_autocmd("ColorScheme", {
  pattern = "gruvbox",
  callback = function()
    if vim.o.background == "dark" then
      vim.cmd([[
          hi! Normal guibg=none guifg=#ffffff
          hi! NormalFloat guibg=none guifg=#ffffff
          hi! Terminal guibg=none guifg=#ffffff
        ]])
    end
  end,
})

vim.g.gruvbox_contrast_dark = "hard"

require("rose-pine").setup {
  styles = { bold = false, italic = false, transparency = true },
}

vim.cmd.colorscheme(os.getenv("NVIM_THEME") or "catppuccin-mocha")

-- -----------------------------------------------------------------------------
-- Statusline
-- -----------------------------------------------------------------------------

-- Which-key
require("which-key").setup {}

-- Fzf-Lua
require("fzf-lua").setup { "telescope" }
FzfLua = require("fzf-lua")
vim.keymap.set("n", "<leader><leader>", FzfLua.files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>I", function() FzfLua.files { cwd = "~/dev/dotfiles" } end, { desc = "Find Configuration" })
vim.keymap.set("n", "<leader>pf", FzfLua.git_files, { desc = "Git Files" })
vim.keymap.set("n", "<leader>k", FzfLua.buffers, { desc = "Buffers" })
vim.keymap.set("n", "<leader>j", FzfLua.live_grep, { desc = "Grep" })
vim.keymap.set("n", "<leader>h", FzfLua.helptags, { desc = "Help Tags" })
vim.keymap.set("n", "<leader>l", FzfLua.diagnostics_document, { desc = "Diagnostics Document" })
vim.keymap.set("n", "<leader>L", FzfLua.diagnostics_workspace, { desc = "Diagnostics Workspace" })
vim.keymap.set("n", "<leader>;", FzfLua.commands, { desc = "Commands" })
vim.keymap.set("n", "<leader>n", function() FzfLua.files { cwd = "~/dev/notes" } end, { desc = "Notes" })
vim.keymap.set({ "n", "v", "x" }, "<leader>J", FzfLua.grep_cword, { desc = "Grep Word" })
FzfLua.register_ui_select()

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local opts = { buffer = args.buf }
    vim.keymap.set("n", "gd", FzfLua.lsp_definitions, opts)
    vim.keymap.set("n", "grr", FzfLua.lsp_references, opts)
    vim.keymap.set("n", "gri", FzfLua.lsp_implementations, opts)
    vim.keymap.set("n", "C", FzfLua.lsp_code_actions, opts)
    vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf, desc = "Open Floating Diagnostic" })
  end,
})

-- Diffview
require("diffview").setup {}
vim.keymap.set("n", "<leader>G", "<cmd>DiffviewOpen<CR>")

-- Gitsigns
local gitsigns = require("gitsigns")
gitsigns.setup {
  current_line_blame = true,
  on_attach = function(bufnr)
    vim.keymap.set("n", "<leader>gb", gitsigns.blame_line, { buffer = bufnr, desc = "Blame Line" })
    vim.keymap.set("n", "<leader>gp", gitsigns.preview_hunk, { buffer = bufnr, desc = "Preview Hunk" })
    vim.keymap.set("n", "<leader>gn", gitsigns.next_hunk, { buffer = bufnr, desc = "Next Hunk" })
    vim.keymap.set("n", "<leader>gN", gitsigns.prev_hunk, { buffer = bufnr, desc = "Prev Hunk" })
  end,
}

-- LSP
require("mason").setup {}
require("mason-lspconfig").setup {
  ensure_installed = { "lua_ls", "gopls", "ts_ls" },
}

vim.lsp.config("lua_ls", {
  settings = {
    Lua = {
      diagnostics = { globals = { "vim" } },
      workspace = { library = vim.api.nvim_get_runtime_file("", true) },
    },
  },
})

-- Conform
require("conform").setup {
  formatters_by_ft = {
    php = nil,
    go = { "goimports" },
    json = { "jq" },
    jsonc = { "jq" },
    astro = { "oxfmt", "biome", "prettierd", stop_after_first = true },
    javascript = { "oxfmt", "biome", "prettierd", stop_after_first = true },
    typescript = { "oxfmt", "biome", "prettierd", stop_after_first = true },
    javascriptreact = { "oxfmt", "biome", "prettierd", stop_after_first = true },
    typescriptreact = { "oxfmt", "biome", "prettierd", stop_after_first = true },
    svelte = { "oxfmt", "prettierd", stop_after_first = true },
    lua = { "stylua" },
  },
  format_on_save = {
    timeout_ms = 500,
    lsp_fallback = false,
  },
  formatters = {
    oxfmt = {
      condition = function(_, ctx)
        return vim.fs.find({ ".oxfmtrc.json", ".oxfmtrc.jsonc" }, {
          path = ctx.filename,
          upward = true,
          stop = vim.uv.os_homedir(),
        })[1] ~= nil
      end,
    },
    biome = {
      condition = function(_, ctx)
        return vim.fs.find({ "biome.json", "biome.jsonc" }, {
          path = ctx.filename,
          upward = true,
          stop = vim.uv.os_homedir(),
        })[1] ~= nil
      end,
    },
    prettierd = {
      condition = function(_, ctx)
        return vim.fs.find({
          ".prettierrc",
          ".prettierrc.json",
          ".prettierrc.js",
          ".prettierrc.cjs",
          ".prettierrc.mjs",
          "prettier.config.js",
          "prettier.config.cjs",
          "prettier.config.mjs",
        }, {
          path = ctx.filename,
          upward = true,
          stop = vim.uv.os_homedir(),
        })[1] ~= nil
      end,
    },
  },
}

vim.api.nvim_create_user_command(
  "Format",
  function()
    require("conform").format {
      bufnr = vim.api.nvim_get_current_buf(),
      timeout_ms = 500,
      lsp_fallback = false,
    }
  end,
  { desc = "Format current buffer using conform" }
)

vim.api.nvim_create_user_command("Json", function() vim.bo.filetype = "json" end, { desc = "Set buffer filetype to JSON" })

-- Treesitter
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

-- Lint
require("lint").linters_by_ft = {
  typescript = { "eslint_d" },
  typescriptreact = { "eslint_d" },
  go = { "golangcilint" },
}
vim.api.nvim_create_autocmd({ "BufWritePost", "BufReadPost", "BufEnter", "FocusGained" }, {
  callback = function() pcall(require("lint").try_lint) end,
})

-- Blink CMP
require("blink.cmp").setup {
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
}

local end_time = vim.uv.hrtime()
print(string.format("Neovim startup took %.2f ms", (end_time - start_time) / 1e6))
