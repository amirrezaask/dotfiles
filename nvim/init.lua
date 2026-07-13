vim.o.number = true
vim.o.relativenumber = true
vim.o.tabstop = 1
vim.o.softtabstop = 1
vim.o.shiftwidth = 1
vim.o.smartindent = true
vim.o.wrap = true
vim.o.swapfile = false
vim.o.backup = false
vim.o.signcolumn = "yes"
vim.o.undofile = true
vim.o.incsearch = true
vim.o.wildoptions = vim.o.wildoptions .. ",fuzzy"
vim.o.clipboard = "unnamedplus"
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.winborder = "rounded"
vim.o.completeopt = "menuone,noselect,popup"

vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.keymap.set("i", "jk", "<esc>")
vim.keymap.set("i", "kj", "<esc>")
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("v", "<", "<gv", { desc = "Indent Left" })
vim.keymap.set("v", ">", ">gv", { desc = "Indent Right" })
vim.keymap.set("v", "p", '"_dP', { desc = "Paste (no yank)" })
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("n", "<leader>i", ":edit $MYVIMRC<CR>")
vim.keymap.set("n", "<C-q>", function()
 if vim.fn.getqflist({ winid = 0 }).winid ~= 0 then
  vim.cmd.cclose()
 else
  vim.cmd.copen()
 end
end)
vim.keymap.set("n", "<leader>q", function() vim.diagnostic.setloclist { open = true } end)

vim.keymap.set("n", "<CR>", function()
 if vim.v.hlsearch == 1 then
  vim.cmd.nohl()
  return ""
 end
 return vim.keycode("<CR>")
end, {
 expr = true,
})

vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter", "CursorHold" }, { callback = function() vim.cmd("checktime") end })
vim.api.nvim_create_autocmd("FileType", { callback = function(args) pcall(vim.treesitter.start, args.buf) end })
vim.api.nvim_create_autocmd("TextYankPost", { callback = function() vim.hl.hl_op { higroup = "Visual", timeout = 150 } end })
vim.api.nvim_create_autocmd("BufEnter", {
 callback = function(args)
  if vim.bo[args.buf].buftype == "prompt" then vim.bo[args.buf].autocomplete = false end
 end,
})
vim.api.nvim_create_autocmd("DiagnosticChanged", { callback = function(_) vim.diagnostic.setqflist { open = false } end })
vim.api.nvim_create_autocmd("VimResized", { command = "wincmd =" })
vim.api.nvim_create_autocmd("BufReadPost", {
 callback = function(args)
  local mark = vim.api.nvim_buf_get_mark(args.buf, '"')
  local line_count = vim.api.nvim_buf_line_count(args.buf)
  if mark[1] > 0 and mark[1] <= line_count then
   vim.api.nvim_win_set_cursor(0, mark)
   vim.schedule(function() vim.cmd("normal! zz") end)
  end
 end,
})

vim.pack.add { -- Installing plugins
 { src = "https://github.com/rose-pine/neovim", name = "rose-pine" },
 { src = "https://github.com/vague-theme/vague.nvim", name = "vague" },
 { src = "https://github.com/ellisonleao/gruvbox.nvim", name = "gruvbox" },
 "https://github.com/brenoprata10/nvim-highlight-colors",
 "https://github.com/ibhagwan/fzf-lua",
 "https://github.com/stevearc/oil.nvim",
 "https://github.com/tpope/vim-fugitive",
 "https://github.com/neovim/nvim-lspconfig",
 "https://github.com/mason-org/mason.nvim",
 "https://github.com/stevearc/conform.nvim",
 "https://github.com/nvim-treesitter/nvim-treesitter",
 "https://github.com/mfussenegger/nvim-lint",
 "https://github.com/saghen/blink.cmp",
}

vim.g.transparency = os.getenv("NVIM_TRANSPARENCY") or false
require("rose-pine").setup { styles = { italic = false, transparency = vim.g.transparency } }
require("vague").setup { transparent = vim.g.transparency, italic = false }
require("gruvbox").setup {
 contrast = "hard",
 italic = {
  strings = false,
  emphasis = false,
  comments = false,
  operators = false,
  folds = false,
 },
 transparent_mode = vim.g.transparency,
}

vim.cmd.colorscheme(os.getenv("NVIM_THEME") or "gruvbox")

require("nvim-highlight-colors").setup {}

local fzf = require("fzf-lua")

fzf.setup { "fzf-vim", winopts = { height = 1, width = 1, border = "none" } }
fzf.register_ui_select()

vim.keymap.set("n", "<leader><leader>", fzf.files, { desc = "Find Files" })
vim.keymap.set("n", "<C-p>", fzf.files, { desc = "Git Files" })
vim.keymap.set("n", "<leader>l", fzf.lines, { desc = "Buffer Lines" })
vim.keymap.set("n", "<leader>f", fzf.git_files, { desc = "Git Files" })
vim.keymap.set("n", "<leader>j", fzf.live_grep, { desc = "Grep" })
vim.keymap.set("n", "<leader>J", fzf.grep_cword, { desc = "Grep Word" })
vim.keymap.set("v", "<leader>J", fzf.grep_visual, { desc = "Grep Word" })
vim.keymap.set("n", "<leader>k", fzf.buffers, { desc = "Buffers" })

vim.api.nvim_create_autocmd("LspAttach", {
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

-- require("oil").setup {
--  float = { win_options = { winblend = 0 }, get_win_title = nil, preview_split = "auto", override = function(conf) return conf end },
-- }
-- vim.keymap.set("n", "<leader>e", require("oil").toggle_float, { desc = "Toggle floating Oil window" })

local mason_bin = vim.fn.stdpath("data") .. "/mason/bin"
vim.env.PATH = mason_bin .. ":" .. (vim.env.PATH or "")

require("mason").setup {}

vim.lsp.config(
 "lua_ls",
 { settings = { Lua = { diagnostics = { globals = { "vim" } }, workspace = { library = vim.api.nvim_get_runtime_file("", true) } } } }
)

vim.lsp.enable { "gopls", "lua_ls", "ts_ls", "rust_analyzer", "clangd" }

vim.api.nvim_create_autocmd("LspAttach", {
 callback = function(args)
  vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf, desc = "Open Floating Diagnostic" })
  vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf, desc = "Code Actions" })
 end,
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
  lsp_format = "never",
 },
 format_after_save = function(buffer_number)
  if vim.g.disable_autoformat or vim.b[buffer_number].disable_autoformat then return end
  return {
   async = true,
   timeout_ms = 500,
   lsp_format = "never",
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

vim.api.nvim_create_user_command("Format", function() require("conform").format {} end, { desc = "Format current buffer using conform" })
vim.api.nvim_create_user_command("Json", function() vim.bo.filetype = "json" end, { desc = "Set buffer filetype to JSON" })

require("lint").linters_by_ft = {
 typescript = { "eslint_d", "oxlint" },
 typescriptreact = { "eslint_d", "oxlint" },
 go = { "golangcilint" },
}

vim.api.nvim_create_autocmd(
 { "BufWritePost", "BufReadPost", "BufEnter", "FocusGained" },
 { callback = function() pcall(require("lint").try_lint) end }
)

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
