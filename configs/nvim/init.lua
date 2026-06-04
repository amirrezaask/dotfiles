local start_time = vim.uv.hrtime()
vim.g.transparency = os.getenv("NVIM_TRANSPARENCY") or true

--{{{
vim.opt.nu = true
vim.opt.relativenumber = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.wrap = false
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = true
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.termguicolors = true
vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.isfname:append("@-@")
vim.opt.updatetime = 50
vim.o.clipboard = "unnamedplus"
vim.opt.wildoptions:append("fuzzy")
vim.o.pumheight = 10
vim.o.pumblend = 10
vim.o.laststatus = 0 -- Experimental: disables the statusbar
vim.o.winbar = "%m%r%h%f"
vim.o.title = true
vim.o.titlestring = "%{fnamemodify(getcwd(), ':~')}"
vim.o.shortmess = vim.o.shortmess .. "I" .. "W" .. "C"
vim.o.cursorline = true

local ok, ui2 = pcall(require, "vim._core.ui2")
if ok then
  ui2.enable { enable = true }
  vim.o.cmdheight = 0
  vim.pack.add { "https://github.com/rachartier/tiny-cmdline.nvim" }
end

--- }}}

-- [keymaps] {{{
vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.keymap.set("i", "jk", "<esc>")
vim.keymap.set("i", "kj", "<esc>")
vim.keymap.set("i", "<C-c>", "<esc>")

vim.keymap.set({ "n", "x", "o" }, "H", "^", { desc = "Start of Line" })
vim.keymap.set({ "n", "x", "o" }, "L", "g_", { desc = "End of Line" })

vim.keymap.set("v", "<", "<gv", { desc = "Indent Left" })
vim.keymap.set("v", ">", ">gv", { desc = "Indent Right" })
vim.keymap.set("v", "p", '"_dP', { desc = "Paste (no yank)" })

vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")

vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")

vim.keymap.set("n", "<leader>i", ":edit $MYVIMRC<CR>", { desc = "Edit Configuration" })
vim.keymap.set("n", "<leader>R", ":source $MYVIMRC<CR>", { desc = "Reload Configuration" })
vim.keymap.set("n", "<leader>t", ":edit ~/TODO.md<CR>", { desc = "Edit TODO.md" })

local term_normal = [[<C-\><C-n>]]
vim.keymap.set("t", "<Esc>", term_normal, { desc = "Leave terminal insert mode" })
vim.keymap.set("t", "jk", term_normal, { desc = "Leave terminal insert mode" })
vim.keymap.set("t", "<C-h>", term_normal .. "<C-w>h", { desc = "Window left (from terminal)" })
vim.keymap.set("t", "<C-j>", term_normal .. "<C-w>j", { desc = "Window down (from terminal)" })
vim.keymap.set("t", "<C-k>", term_normal .. "<C-w>k", { desc = "Window up (from terminal)" })
vim.keymap.set("t", "<C-l>", term_normal .. "<C-w>l", { desc = "Window right (from terminal)" })
-- }}}

require("project").setup { projects_dir = "~/dev" }
vim.keymap.set("n", "<leader>p", require("project").switch, { desc = "Switch projects" })

vim.keymap.set("n", "<C-q>", function()
  if vim.fn.getqflist({ winid = 0 }).winid ~= 0 then
    vim.cmd.cclose()
  else
    vim.cmd.copen()
  end
end, {
  desc = "Toggle quickfix list",
})
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
-- }}}

-- [autocmds for better editor experience] {{{
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter", "CursorHold" }, {
  callback = function() vim.cmd("checktime") end,
})

vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function() vim.hl.hl_op { higroup = "Visual", timeout = 150 } end,
})

vim.api.nvim_create_autocmd("BufEnter", {
  callback = function(args)
    if vim.bo[args.buf].buftype == "prompt" then vim.bo[args.buf].autocomplete = false end
  end,
})

vim.api.nvim_create_autocmd("VimResized", {
  command = "wincmd =",
})

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

-- }}}

-- [colors] {{{
vim.pack.add {
  "https://github.com/folke/tokyonight.nvim",
  "https://github.com/vague-theme/vague.nvim",
  { src = "https://github.com/catppuccin/nvim", name = "catppuccin" },
  { src = "https://github.com/rose-pine/neovim", name = "rose-pine" },
  "https://github.com/scottmckendry/cyberdream.nvim",
}

vim.api.nvim_create_autocmd("ColorScheme", {
  pattern = "default",
  callback = function(args)
    if args.match == "default" then vim.cmd([[
        hi! Normal guibg=none
        hi! link SnacksPickerDir Normal
      ]]) end
  end,
})
require("vague").setup { bold = false, italic = false, transparent = vim.g.transparency }
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
require("rose-pine").setup {
  styles = { bold = false, italic = false, transparency = vim.g.transparency },
}

vim.cmd.colorscheme(os.getenv("NVIM_THEME") or "rose-pine-moon")
-- }}}

-- [editor] {{{
vim.pack.add {
  "https://github.com/tpope/vim-sleuth",
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
vim.pack.add { "https://github.com/saghen/blink.cmp" }

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

-- [snacks] {{{
vim.pack.add { "https://github.com/folke/snacks.nvim" }

Snacks = require("snacks")

require("snacks").setup {
  bigfile = { enabled = true },
  indent = { enabled = true },
  input = { enabled = true },
  picker = { enabled = true },
  notifier = { enabled = true },
  quickfile = { enabled = true },
  statuscolumn = { enabled = true },
  image = { enabled = false },
}

vim.keymap.set("n", "<leader><leader>", Snacks.picker.files, { desc = "Find Files" })
vim.keymap.set("n", "<C-p>", Snacks.picker.files, { desc = "Git Files" })
vim.keymap.set("n", "<leader>l", Snacks.picker.lines, { desc = "Buffer Lines" })
vim.keymap.set("n", "<leader>gf", Snacks.picker.git_files, { desc = "Git Files" })
vim.keymap.set("n", "<leader>gl", Snacks.picker.git_log, { desc = "Git Log" })
vim.keymap.set("n", "<leader>gL", Snacks.picker.git_log_line, { desc = "Git Log Line" })
vim.keymap.set("n", "<leader>j", Snacks.picker.grep, { desc = "Grep" })
vim.keymap.set({ "n", "v" }, "<leader>J", Snacks.picker.grep_word, { desc = "Grep Word" })
vim.keymap.set("n", "<leader>k", function() Snacks.picker.buffers { hidden = false } end, { desc = "Buffers" })

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    vim.keymap.set("n", "gd", Snacks.picker.lsp_definitions, { buffer = args.buf, desc = "[g]oto [d]efinition" })
    vim.keymap.set("n", "grr", Snacks.picker.lsp_references, { buffer = args.buf, desc = "[g]oto [r]eferences" })
    vim.keymap.set("n", "gri", Snacks.picker.lsp_implementations, { buffer = args.buf, desc = "[g]oto [i]mplmentations" })
    vim.keymap.set("n", "gO", Snacks.picker.lsp_symbols, { buffer = args.buf, desc = "[g]oto symbol" })
    vim.keymap.set("n", "<leader>o", Snacks.picker.lsp_symbols, { buffer = args.buf, desc = "[s]ymbols (outline)" })
    vim.keymap.set("n", "<leader>O", Snacks.picker.lsp_workspace_symbols, { buffer = args.buf, desc = "[s]ymbols [w]orkspace" })
    vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf, desc = "Code Actions" })
  end,
})
-- }}}

-- [oil] {{{
vim.pack.add { "https://github.com/stevearc/oil.nvim" }

require("oil").setup {
  float = {
    win_options = {
      winblend = 5,
    },
    get_win_title = nil,
    preview_split = "auto",
    override = function(conf) return conf end,
  },
}

vim.keymap.set("n", "<leader>e", require("oil").toggle_float, { desc = "Toggle floating Oil window" })
-- }}}

-- [git] {{{
vim.pack.add {
  "https://github.com/tpope/vim-fugitive",
  "https://github.com/lewis6991/gitsigns.nvim",
  "https://github.com/sindrets/diffview.nvim",
}

require("diffview").setup {}
vim.keymap.set("n", "<leader>G", "<cmd>DiffviewOpen<CR>")
vim.keymap.set("n", "<leader>gr", "<cmd>DiffviewRefresh<CR>", { desc = "Diffview Refresh" })

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
-- }}}

-- [lsp] {{{
vim.pack.add {
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
      workspace = { library = vim.api.nvim_get_runtime_file("", true) },
    },
  },
})

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf, desc = "Open Floating Diagnostic" })
    vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf, desc = "Code Actions" })
  end,
})

-- }}}

-- [conform: Autoformat] {{{
vim.pack.add { "https://github.com/stevearc/conform.nvim" }

vim.api.nvim_create_user_command("ConformDisable", function(args)
  if args.bang then
    vim.b.disable_autoformat = true
  else
    vim.g.disable_autoformat = true
  end
end, {
  desc = "Disable conform-autoformat-on-save",
  bang = true,
})

vim.api.nvim_create_user_command("ConformEnable", function()
  vim.b.disable_autoformat = false
  vim.g.disable_autoformat = false
end, {
  desc = "Re-enable conform-autoformat-on-save",
})

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
-- }}}

-- [treesitter] {{{
vim.pack.add {
  "https://github.com/nvim-treesitter/nvim-treesitter",
  "https://github.com/nvim-treesitter/nvim-treesitter-context",
}

vim.api.nvim_create_autocmd("FileType", { callback = function(args) pcall(vim.treesitter.start, args.buf) end })

require("treesitter-context").setup { enable = true }

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
vim.pack.add { "https://github.com/mfussenegger/nvim-lint" }

require("lint").linters_by_ft = {
  typescript = { "eslint_d" },
  typescriptreact = { "eslint_d" },
  go = { "golangcilint" },
}

vim.api.nvim_create_autocmd({ "BufWritePost", "BufReadPost", "BufEnter", "FocusGained" }, {
  callback = function() pcall(require("lint").try_lint) end,
})
-- }}}

print(string.format("Neovim startup took %.2f ms", (vim.uv.hrtime() - start_time) / 1e6))
