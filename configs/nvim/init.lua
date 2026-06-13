local aucmd = vim.api.nvim_create_autocmd
local command = vim.api.nvim_create_user_command
local set = vim.keymap.set
local o = vim.o
local g = vim.g
local add = vim.pack.add
local a = vim.api

-- options {{{
o.nu = true
o.relativenumber = true
o.guicursor = "n-v-c-sm-i-ci-ve:block,r-cr-o:hor20,t:block-blinkon500-blinkoff500-TermCursor"
o.tabstop = 1
o.softtabstop = 1
o.shiftwidth = 1
o.expandtab = true
o.smartindent = true
o.wrap = true
o.mousescroll = "ver:5,hor:0"
o.swapfile = false
o.backup = false
o.undodir = os.getenv("HOME") .. "/.vim/undodir"
o.undofile = true
o.hlsearch = false
o.incsearch = true
o.termguicolors = true
o.scrolloff = 8
o.signcolumn = "yes"
o.wildoptions = o.wildoptions .. ",fuzzy"
o.updatetime = 50
o.clipboard = "unnamedplus"
o.pumheight = 10
o.pumblend = 10
o.statusline = "%m%r%h%f%=%y %l:%c "
o.splitbelow = true
o.splitright = true
o.title = true
o.titlestring = "%{fnamemodify(getcwd(), ':~')}"
o.shortmess = vim.o.shortmess .. "I" .. "W" .. "C"
o.cursorline = false
o.winborder = "rounded"
g.mapleader = " "
g.maplocalleader = " "
--- }}}

-- [keymaps] {{{
set("i", "jk", "<esc>")
set("i", "kj", "<esc>")
set("i", "<C-c>", "<esc>")
set("v", "<", "<gv", { desc = "Indent Left" })
set("v", ">", ">gv", { desc = "Indent Right" })
set("v", "p", '"_dP', { desc = "Paste (no yank)" })
set("n", "<C-d>", "<C-d>zz")
set("n", "<C-u>", "<C-u>zz")
set("n", "n", "nzz")
set("n", "N", "Nzz")
set("n", "j", "gj")
set("n", "k", "gk")

local term_normal = [[<C-\><C-n>]]
set("t", "<Esc>", term_normal, { desc = "Leave terminal insert mode" })
set("t", "jk", term_normal, { desc = "Leave terminal insert mode" })
set("t", "<C-h>", term_normal .. "<C-w>h", { desc = "Window left (from terminal)" })
set("t", "<C-j>", term_normal .. "<C-w>j", { desc = "Window down (from terminal)" })
set("t", "<C-k>", term_normal .. "<C-w>k", { desc = "Window up (from terminal)" })
set("t", "<C-l>", term_normal .. "<C-w>l", { desc = "Window right (from terminal)" })
-- }}}

set("n", "<C-q>", function()
 if vim.fn.getqflist({ winid = 0 }).winid ~= 0 then
  vim.cmd.cclose()
 else
  vim.cmd.copen()
 end
end, {
 desc = "Toggle quickfix list",
})
set("n", "<leader>q", function() vim.diagnostic.setloclist { open = true } end)

set("n", "<CR>", function()
 if vim.v.hlsearch == 1 then
  vim.cmd.nohl()
  return ""
 end
 return vim.keycode("<CR>")
end, {
 expr = true,
})
-- }}}

-- [autocmds for better editor experience] {{{
aucmd({ "FocusGained", "BufEnter", "CursorHold" }, {
 callback = function() vim.cmd("checktime") end,
})

aucmd("TextYankPost", {
 callback = function() vim.hl.hl_op { higroup = "Visual", timeout = 150 } end,
})

aucmd("BufEnter", {
 callback = function(args)
  if vim.bo[args.buf].buftype == "prompt" then vim.bo[args.buf].autocomplete = false end
 end,
})

aucmd("DiagnosticChanged", {
 callback = function(_) vim.diagnostic.setqflist { open = false } end,
})

aucmd("VimResized", {
 command = "wincmd =",
})

aucmd("BufReadPost", {
 callback = function(args)
  local mark = a.nvim_buf_get_mark(args.buf, '"')
  local line_count = a.nvim_buf_line_count(args.buf)
  if mark[1] > 0 and mark[1] <= line_count then
   a.nvim_win_set_cursor(0, mark)
   vim.schedule(function() vim.cmd("normal! zz") end)
  end
 end,
})
-- }}}

-- [colors] {{{
vim.g.transparency = os.getenv("NVIM_TRANSPARENCY") or true
add {
 { src = "https://github.com/folke/tokyonight.nvim", name = "tokyonight" },
 { src = "https://github.com/catppuccin/nvim", name = "catppuccin" },
 { src = "https://github.com/rose-pine/neovim", name = "rose-pine" },
 { src = "https://github.com/vague-theme/vague.nvim", name = "vague" },
}

require("rose-pine").setup {
 styles = { italic = false, transparency = vim.g.transparency },
}

require("vague").setup {
 transparent = vim.g.transparency,
 italic = false,
}

require("tokyonight").setup {
 transparent = vim.g.transparency,
 styles = { comments = { italic = false }, keywords = { italic = false } },
}

require("catppuccin").setup {
 transparent_background = vim.g.transparency,
 background = { -- :h background
  light = "macchiato",
  dark = "mocha",
 },
}

-- For vercel theme checkout themes directory in the root of dotfiles.
vim.cmd.colorscheme(os.getenv("NVIM_THEME") or "vague")
-- }}}

-- [color highlight] {{{
add {
 "https://github.com/brenoprata10/nvim-highlight-colors",
}

require("nvim-highlight-colors").setup {
 render = "background",
 enable_hex = true,
 enable_short_hex = true,
 enable_rgb = true,
 enable_hsl = true,
 enable_var_usage = true,
 enable_named_colors = true,
 enable_tailwind = true,
}
-- }}}

-- [blink.cmp] {{{
add { "https://github.com/saghen/blink.cmp" }

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
 sources = {
  default = { "lsp", "path", "buffer" },
  providers = {
   lsp = {
    score_offset = 1000,
   },
   path = {
    score_offset = 3,
   },
   buffer = {
    score_offset = -150,
    min_keyword_length = 3,
   },
  },
 },
 completion = {
  accept = { auto_brackets = { enabled = true } },
  menu = {
   border = "rounded",
   max_height = 10,
   draw = {
    columns = {
     { "kind_icon" },
     { "label", "label_description", gap = 1 },
     { "source_name" },
    },
    components = {
     source_name = {
      text = function(ctx)
       local source_names = {
        lsp = "[LSP]",
        buffer = "[Buffer]",
        path = "[Path]",
        snippets = "[Snippet]",
       }
       return (source_names[ctx.source_name] or "[") .. ctx.source_name .. "]"
      end,
      highlight = "CmpItemMenu",
     },
    },
   },
   auto_show = true,
  },
  documentation = { auto_show = true, auto_show_delay_ms = 200 },
  ghost_text = { enabled = true },
 },
}
-- }}}

-- [fzf-lua] {{{
add { "https://github.com/ibhagwan/fzf-lua" }

local fzf = require("fzf-lua")

fzf.setup {
 "telescope",
 winopts = {
  -- Full screen fzf
  height = 1,
  width = 1,
 },
}

fzf.register_ui_select()

vim.keymap.set("n", "<leader><leader>", fzf.files, { desc = "Find Files" })
vim.keymap.set("n", "<C-p>", fzf.files, { desc = "Git Files" })
vim.keymap.set("n", "<leader>l", fzf.lines, { desc = "Buffer Lines" })
vim.keymap.set("n", "<leader>f", fzf.git_files, { desc = "Git Files" })
vim.keymap.set("n", "<leader>j", fzf.live_grep, { desc = "Grep" })
vim.keymap.set("n", "<leader>J", fzf.grep_cword, { desc = "Grep Word" })
vim.keymap.set("v", "<leader>J", fzf.grep_visual, { desc = "Grep Word" })
vim.keymap.set("n", "<leader>k", fzf.buffers, { desc = "Buffers" })

command("GitCommits", FzfLua.git_commits, {})
command("GitBCommits", FzfLua.git_bcommits, {})

a.nvim_create_autocmd("LspAttach", {
 callback = function(args)
  vim.keymap.set("n", "gd", fzf.lsp_definitions, { buffer = args.buf, desc = "[g]oto [d]efinition" })
  vim.keymap.set("n", "grr", fzf.lsp_references, { buffer = args.buf, desc = "[g]oto [r]eferences" })
  vim.keymap.set("n", "gri", fzf.lsp_implementations, { buffer = args.buf, desc = "[g]oto [i]mplmentations" })
  vim.keymap.set("n", "gO", fzf.lsp_document_symbols, { buffer = args.buf, desc = "[g]oto symbol" })
  vim.keymap.set("n", "<leader>o", fzf.lsp_document_symbols, { buffer = args.buf, desc = "[s]ymbols (outline)" })
  vim.keymap.set("n", "<leader>O", fzf.lsp_workspace_symbols, { buffer = args.buf, desc = "[s]ymbols [w]orkspace" })
  vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf, desc = "Code Actions" })
 end,
})
-- }}}

-- [oil] {{{
add { "https://github.com/stevearc/oil.nvim" }

require("oil").setup {
 float = { win_options = { winblend = 0 }, get_win_title = nil, preview_split = "auto", override = function(conf) return conf end },
}

vim.keymap.set("n", "<leader>e", require("oil").toggle_float, { desc = "Toggle floating Oil window" })
-- }}}

-- [git] {{{
add { "https://github.com/tpope/vim-fugitive" }
-- }}}

-- [lsp] {{{
add {
 "https://github.com/neovim/nvim-lspconfig",
 "https://github.com/mason-org/mason.nvim",
 "https://github.com/mason-org/mason-lspconfig.nvim",
}

require("mason").setup {}
require("mason-lspconfig").setup {
 ensure_installed = { "lua_ls", "gopls", "ts_ls" },
}

vim.lsp.config("lua_ls", {
 settings = {
  Lua = {
   diagnostics = { globals = { "vim" } },
   workspace = { library = a.nvim_get_runtime_file("", true) },
  },
 },
})

a.nvim_create_autocmd("LspAttach", {
 callback = function(args)
  vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf, desc = "Open Floating Diagnostic" })
  vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf, desc = "Code Actions" })
 end,
})

-- }}}

-- [conform: Autoformat] {{{
add { "https://github.com/stevearc/conform.nvim" }

a.nvim_create_user_command("ConformDisable", function(args)
 if args.bang then
  vim.b.disable_autoformat = true
 else
  vim.g.disable_autoformat = true
 end
end, {
 desc = "Disable conform-autoformat-on-save",
 bang = true,
})

a.nvim_create_user_command("ConformEnable", function()
 vim.b.disable_autoformat = false
 vim.g.disable_autoformat = false
end, {
 desc = "Re-enable conform-autoformat-on-save",
})

local has_markers = function(markers)
 return function(_, ctx)
  return vim.fs.find(markers, {
   path = ctx.filename,
   upward = true,
   stop = vim.uv.os_homedir(),
  })[1] ~= nil
 end
end
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
 notify_on_error = false,
 default_format_opts = {
  async = true,
  timeout_ms = 500,
  lsp_format = "fallback",
 },
 format_after_save = function(buffer_number)
  if vim.g.disable_autoformat or vim.b[buffer_number].disable_autoformat then return end
  return {
   async = true,
   timeout_ms = 500,
   lsp_format = "fallback",
  }
 end,
 formatters = {
  oxfmt = {
   condition = has_markers { ".oxfmtrc.json", ".oxfmtrc.jsonc" },
  },
  biome = {
   condition = has_markers { "biome.json", "biome.jsonc" },
  },
  prettierd = {
   condition = has_markers {
    ".prettierrc",
    ".prettierrc.json",
    ".prettierrc.js",
    ".prettierrc.cjs",
    ".prettierrc.mjs",
    "prettier.config.js",
    "prettier.config.cjs",
    "prettier.config.mjs",
   },
  },
 },
}

command(
 "Format",
 function()
  require("conform").format {
   bufnr = a.nvim_get_current_buf(),
   timeout_ms = 500,
   lsp_fallback = false,
  }
 end,
 { desc = "Format current buffer using conform" }
)

command("Json", function() vim.bo.filetype = "json" end, { desc = "Set buffer filetype to JSON" })
-- }}}

-- [treesitter] {{{
add {
 "https://github.com/nvim-treesitter/nvim-treesitter",
}

aucmd("FileType", { callback = function(args) pcall(vim.treesitter.start, args.buf) end })

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
-- }}}

-- [lint] {{{
add { "https://github.com/mfussenegger/nvim-lint" }

require("lint").linters_by_ft = {
 typescript = { "eslint_d", "oxlint" },
 typescriptreact = { "eslint_d", "oxlint" },
 go = { "golangcilint" },
}

aucmd({ "BufWritePost", "BufReadPost", "BufEnter", "FocusGained" }, {
 callback = function() pcall(require("lint").try_lint) end,
})

add { "https://github.com/rachartier/tiny-inline-diagnostic.nvim" }
require("tiny-inline-diagnostic").setup {
 preset = "powerline",
 options = {
  add_messages = {
   display_count = true,
   messages = true,
  },
  multilines = {
   always_show = true,
   enabled = true,
  },
 },
}

vim.diagnostic.config { virtual_text = false } -- Disable Neovim's default virtual text diagnostics, in favor of the tiny inline diagnostic
-- }}}
