--    ___         _                      ___       __
--   / _ | __ _  (_)__________ ___ ___ _/ _ | ___ / /__
--  / __ |/  ' \/ / __/ __/ -_)_ // _ `/ __ |(_-</  '_/
-- /_/ |_/_/_/_/_/_/ /_/  \__//__/\_,_/_/ |_/___/_/\_\
--
local function packer_ensure()
  local fn = vim.fn
  local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system { "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path }
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

-- If packer.nvim is not installed, install it.
packer_ensure()

-- [[ Basic Options
vim.opt.ignorecase = true -- ignore case when searching
vim.opt.smartcase = true --don't ignore case in search when there is uppercase letter
vim.opt.equalalways = false -- don't resize windows on changing window state ( closing or splitting )
vim.opt.modeline = true --
vim.opt.autoread = true -- If you detect a file change read it automatically
vim.opt.compatible = false -- no need legacy VI compatibility
vim.opt.encoding = "utf-8" -- Default file encoding
vim.opt.hlsearch = true -- Highlight search matches
vim.opt.history = 700 -- Number of stored history items
vim.opt.tabpagemax = 100 -- Max number of tabs
vim.opt.ruler = true -- Show line and column in statusline
vim.opt.mouse = "a" -- enable mouse support for all modes
vim.opt.autoindent = true
vim.opt.cindent = true
vim.opt.wrap = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.cursorline = true -- highlight current line
vim.opt.relativenumber = true -- show relative line numbers
vim.opt.number = true -- show current line number
vim.opt.showmode = true -- show current vim mode down of the screen
vim.opt.showcmd = true -- show commands as they are being typed
vim.opt.hidden = true
vim.opt.updatetime = 100
vim.opt.incsearch = true -- Continue search as I type characters
vim.opt.guioptions = "egmrti"
vim.opt.backspace = "indent,eol,start"
vim.opt.complete = vim.opt.complete + "i" -- don't search for all included files
vim.opt.wildmenu = true
vim.opt.wildoptions = "tagfile"
vim.opt.updatetime = 300
vim.opt.pumheight = 10 -- Completion window max size
vim.opt.conceallevel = 2 -- Concealed text is completely hidden
vim.opt.shortmess = vim.opt.shortmess + "c" -- Shut off completion messages
vim.opt.belloff = vim.opt.belloff + "ctrlg" -- If Vim beeps during completion
vim.opt.termguicolors = true
vim.opt.list = true
vim.opt.listchars:append "tab:<->"
vim.opt.listchars:append "eol:↲"
vim.opt.listchars:append "trail:·"
vim.opt.listchars:append "lead:·"
vim.cmd [[ set clipboard^=unnamedplus ]]
-- ]]

-- Keymap helper
vim.g.mapleader = " "

local function bind(spec)
  for mode, keys in pairs(spec) do
    for key, binding in pairs(keys) do
      vim.keymap.set(mode, key, binding)
    end
  end
end

-- [[ Basic keymaps
bind {
  n = {
    ["Q"] = "<NOP>",
    [";,"] = ":",
    ["q;"] = "q:",
    ["{"] = ":cprev<CR>",
    ["}"] = ":cnext<CR>",
    ["<C-h>"] = ":tabprev<CR>",
    ["<C-l>"] = ":tabnext<CR>",
    ["Y"] = "y$",
    ["n"] = "nzz",
    ["N"] = "Nzz",
    ["<M-p>"] = ":bprev<CR>",
    ["<M-n>"] = ":bnext<CR>",
    ["<M-j>"] = ":m .+1<CR>==",
    ["<M-k>"] = ":m .-2<CR>==",
    ["j"] = "gj",
    ["k"] = "gk",
  },
  t = {
    ["<Esc>"] = "<C-\\><C-n>",
    ["jk"] = "<C-\\><C-n>",
    ["kj"] = "<C-\\><C-n>",
  },

  i = {
    ["jk"] = "<esc>",
    ["kj"] = "<esc>",
  },
}

vim.cmd [[ nnoremap <expr><CR> {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]]
-- ]]

-- [[ Plugins
require("packer").startup(function(use)
  use "wbthomason/packer.nvim"

  -- Colorschemes
  use "eemed/sitruuna.vim"
  use "sainnhe/sonokai"
  use "folke/tokyonight.nvim"
  use "shaunsingh/solarized.nvim"
  use "shaunsingh/nord.nvim"
  use "ellisonleao/gruvbox.nvim"
  use { "amirrezaask/gruvbuddy.nvim", requires = "tjdevries/colorbuddy.vim" }

  use {
    "j-hui/fidget.nvim",
    config = function()
      require("fidget").setup()
    end,
  }
  use "junegunn/goyo.vim"
  use "mhinz/vim-startify"
  use "nvim-tree/nvim-web-devicons"
  use "yamatsum/nvim-nonicons"
  use {
    "stevearc/dressing.nvim",
    config = function()
      require("dressing").setup()
    end,
  }
  use {
    "folke/which-key.nvim",
    config = function()
      vim.opt.timeoutlen = 500
      require("which-key").setup()
    end,
  }

  -- LSP
  use "neovim/nvim-lspconfig"
  use {
    "williamboman/mason.nvim",
    config = function()
      require("mason").setup()
    end,
  }

  -- Git stuff
  use "tpope/vim-fugitive"
  use "junegunn/gv.vim"
  use "cohama/agit.vim"
  use {
    "lewis6991/gitsigns.nvim",
    config = function()
      require("gitsigns").setup()
    end,
  }
  use {
    "TimUntersberger/neogit",
    requires = "nvim-lua/plenary.nvim",
    config = function()
      require("neogit").setup()
    end,
  }

  -- Completion
  use "hrsh7th/nvim-cmp" -- Neovim auto complete menu
  use "hrsh7th/vim-vsnip" -- Snippets
  use "hrsh7th/cmp-buffer" -- auto complete buffer source
  use "hrsh7th/cmp-nvim-lua" -- auto complete nvim lua stuff source
  use "hrsh7th/cmp-nvim-lsp" -- auto complete lsp source
  use "hrsh7th/cmp-path" -- auto complete os path source

  -- Treesitter
  use "nvim-treesitter/nvim-treesitter"
  use "nvim-treesitter/nvim-treesitter-textobjects"
  use "p00f/nvim-ts-rainbow"

  -- Telescope
  use { "nvim-telescope/telescope.nvim", requires = "nvim-lua/plenary.nvim" }
  use { "nvim-telescope/telescope-file-browser.nvim" }

  use "sheerun/vim-polyglot"

  use "towolf/vim-helm"
  use "windwp/nvim-spectre"
  use "pbrisbin/vim-mkdir"
  use "tpope/vim-commentary"
  use "tpope/vim-surround"
  use "junegunn/vim-easy-align"
  use "fladson/vim-kitty"
  use {
    "folke/todo-comments.nvim",
    requires = "nvim-lua/plenary.nvim",
    config = function()
      require("todo-comments").setup()
    end,
  }

  use "ziglang/zig.vim"
  use "jansedivy/jai.vim"

  use { "ckipp01/stylua-nvim", run = "cargo install stylua" }
  use "rust-lang/rust.vim"
  use "simrat39/rust-tools.nvim"

  use "cuducos/yaml.nvim"

  use "Glench/Vim-Jinja2-Syntax"
  use "nvim-lualine/lualine.nvim"
end)
-- ]]

-- [[ Colorscheme
vim.cmd [[ colorscheme gruvbox ]]
-- ]]

local function onsave(pattern, callback)
  local augroup_name = ""
  if type(pattern) == "table" then
    augroup_name = table.concat(pattern, ",") .. "-onsave"
  end
  if type(pattern) == "string" then
    augroup_name = pattern .. "-onsave"
  end
  vim.api.nvim_create_autocmd("BufWritePost", {
    group = vim.api.nvim_create_augroup(augroup_name, {}),
    pattern = pattern,
    callback = callback,
  })
end

-- [[ LSP
local function lsp_on_attach(_, bufnr)
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  local opts = { noremap = true, silent = true }
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "gd",
    "<cmd>lua vim.lsp.buf.definition()<CR>",
    { silent = true, noremap = true }
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "gi",
    "<cmd>lua vim.lsp.buf.implementation()<CR>",
    { silent = true, noremap = true }
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "gr",
    "<cmd>lua vim.lsp.buf.references()<CR>",
    { silent = true, noremap = true }
  )
  vim.api.nvim_buf_set_keymap(bufnr, "n", "R", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "i", "<c-s>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<c-s>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<c-d>", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "C", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
end

local border = {
  { "🭽", "FloatBorder" },
  { "▔", "FloatBorder" },
  { "🭾", "FloatBorder" },
  { "▕", "FloatBorder" },
  { "🭿", "FloatBorder" },
  { "▁", "FloatBorder" },
  { "🭼", "FloatBorder" },
  { "▏", "FloatBorder" },
}

local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
  opts = opts or {}
  opts.border = opts.border or border
  return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

-- ]]
-- [[ Lua
local sumneko_root = string.format("%s/.local/lua-language-server", os.getenv "HOME")
local sumneko_binary = sumneko_root .. "/bin/lua-language-server"

local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

require("nvim-treesitter.install").ensure_installed "lua"

onsave("*.lua", function()
  require("stylua-nvim").format_file()
end)

vim.api.nvim_create_autocmd("BufEnter", {
  pattern = "*.lua",
  callback = function()
    vim.cmd [[ setlocal sts=2 sw=2 ]]
  end,
})

require("lspconfig").sumneko_lua.setup {
  cmd = { sumneko_binary, "-E", sumneko_root .. "/main.lua" },
  on_attach = lsp_on_attach,
  commands = {
    Format = {
      function()
        require("stylua-nvim").format_file()
      end,
    },
  },
  settings = {
    Lua = {
      diagnostics = {
        globals = { "vim" },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
}
-- ]]

-- [[ Go
require("nvim-treesitter.install").ensure_installed "go"

require("lspconfig").gopls.setup {
  on_attach = lsp_on_attach,
}

onsave("*.go", function()
  vim.lsp.buf.format()
end)
-- ]]

-- [[ PHP
require("nvim-treesitter.install").ensure_installed "php"
require("lspconfig").intelephense.setup {
  on_attach = lsp_on_attach,
}
-- ]]

-- [[ C/C++
require("nvim-treesitter.install").ensure_installed "c"
require("nvim-treesitter.install").ensure_installed "cpp"

require("lspconfig").clangd.setup {
  on_attach = lsp_on_attach,
}
-- ]]

-- [[ Elixir
require("nvim-treesitter.install").ensure_installed "elixir"
require("lspconfig").elixirls.setup {
  on_attach = lsp_on_attach,
  cmd = { os.getenv "HOME" .. "/.local/elixir-ls/language_server.sh" },
}
-- ]]

-- [[ Rust
local rt = require "rust-tools"

require("nvim-treesitter.install").ensure_installed "rust"

rt.setup {
  server = {
    on_attach = function(_, bufnr)
      lsp_on_attach(_, bufnr)
      vim.keymap.set("n", "C", rt.hover_actions.hover_actions, { buffer = bufnr })
      vim.keymap.set("n", "ga", rt.code_action_group.code_action_group, { buffer = bufnr })
    end,
  },
}

onsave("*.rs", function()
  vim.lsp.buf.format()
end)
-- ]]

-- [[ Purescript
require("lspconfig").purescriptls.setup {
  on_attach = lsp_on_attach,
}
--]]

-- [[ Zig
require("nvim-treesitter.install").ensure_installed "zig"
require("lspconfig").zls.setup {
  on_attach = lsp_on_attach,
}
-- ]]

-- [[ Haskell
require("nvim-treesitter.install").ensure_installed "haskell"
require("lspconfig").hls.setup {
  on_attach = lsp_on_attach,
}
-- ]]

-- [[ Python
require("lspconfig").jedi_language_server.setup {
  on_attach = lsp_on_attach,
}
-- ]]

-- [[ Yaml
require("nvim-treesitter.install").ensure_installed "yaml"
vim.cmd [[
    autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
    autocmd FileType yaml setlocal cursorcolumn
]]
-- ]]

-- [[ Lualine
require("lualine").setup {
  options = {
    icons_enabled = true,
    theme = "auto",
    component_separators = { left = "", right = "" },
    section_separators = { left = "", right = "" },
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {},
    always_divide_middle = true,
    globalstatus = false,
    refresh = {
      statusline = 1000,
      tabline = 1000,
      winbar = 1000,
    },
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch", "diff", "diagnostics" },
    lualine_c = { "filename" },
    lualine_x = { "encoding", "fileformat", "filetype" },
    lualine_y = { "progress" },
    lualine_z = { "location" },
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { "filename" },
    lualine_x = { "location" },
    lualine_y = {},
    lualine_z = {},
  },
  tabline = {},
  winbar = {},
  inactive_winbar = {},
  extensions = {},
}
-- ]]

-- [[ nvim-cmp
local cmp = require "cmp"

cmp.setup {
  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
    end,
  },
  mapping = {
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.close(),
    ["<CR>"] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Insert,
      select = false,
    },
    ["<Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end,
    ["<S-Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end,
  },

  sources = {
    { name = "buffer" },
    { name = "nvim_lsp" },
    { name = "path" },
    { name = "nvim_lua" },
  },
}
-- ]]

-- [[ Telescope
require("telescope").setup {
  extensions = {
    file_browser = {
      -- disables netrw and use telescope-file-browser in its place
      hijack_netrw = true,
      mappings = {
        ["i"] = {
          -- your custom insert mode mappings
        },
        ["n"] = {
          -- your custom normal mode mappings
        },
      },
    },
  },
}

bind {
  n = {
    ["<leader><leader>"] = "<cmd>Telescope find_files<CR>",
    ["<leader>h"] = "<cmd>Telescope help_tags<cr>",
    ["<leader>fb"] = "<cmd>Telescope file_browser<CR>",
    ["??"] = "<cmd>Telescope live_grep<CR>",
    ["?a"] = "<cmd>lua vim.lsp.buf.code_action()<CR>",
  },
}

require("telescope").load_extension "file_browser"
-- ]]

-- [[ Treesitter
require("nvim-treesitter.configs").setup {
  highlight = {
    enable = true,
  },
  rainbow = {
    enable = true,
    extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
    max_file_lines = nil, -- Do not enable for files with more than n lines, int
  },
  textobjects = {
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        ["]m"] = "@function.outer",
        ["]]"] = "@class.outer",
      },
      goto_next_end = {
        ["]M"] = "@function.outer",
        ["]["] = "@class.outer",
      },
      goto_previous_start = {
        ["[m"] = "@function.outer",
        ["[["] = "@class.outer",
      },
      goto_previous_end = {
        ["[M"] = "@function.outer",
        ["[]"] = "@class.outer",
      },
    },
    select = {
      enable = true,

      -- Automatically jump forward to textobj, similar to targets.vim
      lookahead = true,

      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
  },
}

-- ]]
