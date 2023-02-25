vim.opt.number = true -- Line numbers
vim.opt.relativenumber = true -- Relative line numbers
vim.opt.errorbells = false
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.wrap = false
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv "HOME" .. "/.vim/undodir"
vim.opt.undofile = true
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.termguicolors = true
vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.isfname:append "@-@"
vim.opt.updatetime = 50
vim.opt.guicursor = ""
vim.opt.shortmess:append "c" -- Don't pass messages to |ins-completion-menu|.
vim.opt.shortmess:append "I" -- No Intro message
vim.opt.clipboard:append "unnamedplus" -- use system clipboard as default register.
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.g.transparent = false

-- Netrw
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25

vim.g.mapleader = " "

vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

vim.keymap.set("n", "<leader>vs", "<cmd>vsplit<CR>")
vim.keymap.set("n", "<leader>sp", "<cmd>split<CR>")

vim.keymap.set("n", "Q", "<NOP>")

vim.keymap.set("n", "{", ":cprev<CR>")
vim.keymap.set("n", "}", ":cnext<CR>")

vim.keymap.set("n", "Y", "y$")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")

vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")

vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })

vim.keymap.set("t", "<Esc>", "<C-\\><C-n>")
vim.keymap.set("t", "jk", "<C-\\><C-n>")
vim.keymap.set("t", "kj", "<C-\\><C-n>")

vim.keymap.set("i", "jk", "<esc>")
vim.keymap.set("i", "kj", "<esc>")

vim.keymap.set({ "n", "t" }, "<C-,>", "<cmd>tabprev<CR>")
vim.keymap.set({ "n", "t" }, "<C-.>", "<cmd>tabnext<CR>")
vim.keymap.set({ "n", "t" }, "<C-t>", "<cmd>tabnew<CR>")

vim.keymap.set("n", "<leader>.", "<cmd>Explore<CR>")

-- Easier split navigation
vim.keymap.set("n", "<C-h>", "<C-w>h")
vim.keymap.set("n", "<C-j>", "<C-w>j")
vim.keymap.set("n", "<C-k>", "<C-w>k")
vim.keymap.set("n", "<C-l>", "<C-w>l")

vim.keymap.set("t", "<C-h>", "<cmd>wincmd h<CR>")
vim.keymap.set("t", "<C-j>", "<cmd>wincmd j<CR>")
vim.keymap.set("t", "<C-k>", "<cmd>wincmd k<CR>")
vim.keymap.set("t", "<C-l>", "<cmd>wincmd l<CR>")

-- Easier split resizing
vim.keymap.set("n", "<A-l>", "<C-w><")
vim.keymap.set("n", "<A-h>", "<C-w>>")
vim.keymap.set("n", "<A-j>", "<C-w>-")
vim.keymap.set("n", "<A-k>", "<C-w>+")

vim.keymap.set("t", "<A-l>", "<C-\\><C-n><C-w><")
vim.keymap.set("t", "<A-h>", "<C-\\><C-n><C-w>>")
vim.keymap.set("t", "<A-j>", "<C-\\><C-n><C-w>-")
vim.keymap.set("t", "<A-k>", "<C-\\><C-n><C-w>+")

vim.keymap.set("n", "<leader>R", function()
  vim.cmd.source "~/.config/nvim/init.lua"
end)

vim.api.nvim_create_user_command("Reload", function(_)
  vim.cmd.source "~/.config/nvim/init.lua"
end, {})

local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system { "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path }
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

ensure_packer()

local function if_has(...)
  for _, mod in ipairs(arg) do
    local has, _ = pcall(require, mod)
    if not has then
      return false
    end
  end

  return true
end

-- Installing Packages
require("packer").startup {
  function(use)
    use "wbthomason/packer.nvim" -- plugin manager itself
    use "lewis6991/impatient.nvim" -- Faster lua require using caching

    use { "folke/tokyonight.nvim" }
    use { "rose-pine/neovim", as = "rose-pine" }
    use { "catppuccin/nvim", as = "catppuccin" }
    use { "dracula/vim", as = "dracula" }
    use { "ellisonleao/gruvbox.nvim" }
    use { "eemed/sitruuna.vim" }

    use { "numToStr/Comment.nvim" } -- Comment code with ease

    use {
      "nvim-telescope/telescope.nvim",
      requires = {
        {
          "nvim-telescope/telescope-fzf-native.nvim",
          run = "make",
        },
        { "nvim-lua/plenary.nvim" },
      },
    } -- Best search interface of all time

    -- Treesitter
    use { "nvim-treesitter/nvim-treesitter" }
    use { "nvim-treesitter/nvim-treesitter-textobjects", after = "nvim-treesitter" }
    use { "neovim/nvim-lspconfig" }
    use { "williamboman/mason.nvim" }
    use { "williamboman/mason-lspconfig.nvim" }

    -- Autocompletion
    use { "hrsh7th/nvim-cmp" }
    use { "hrsh7th/cmp-buffer" }
    use { "hrsh7th/cmp-path" }
    use { "saadparwaiz1/cmp_luasnip" }
    use { "hrsh7th/cmp-nvim-lsp" }
    use { "hrsh7th/cmp-nvim-lua" }
    -- Snippets
    use { "L3MON4D3/LuaSnip" }
    use { "rafamadriz/friendly-snippets" }
    -- Null LS
    use { "jose-elias-alvarez/null-ls.nvim" }

    use "stevearc/oil.nvim" -- File manager like a BOSS
    use "pbrisbin/vim-mkdir" -- Automatically create directory if not exists
    use "fladson/vim-kitty" -- Support Kitty terminal config syntax
    use "towolf/vim-helm" -- Support for helm template syntax
    use "tpope/vim-surround" -- surrounding text objects
    use "kevinhwang91/nvim-bqf" -- Preview quickfix list item.
    use "tpope/vim-eunuch" -- Helper commands like :Rename, :Move, :Delete, :Remove, ...
    use "tpope/vim-sleuth" -- Heuristically set buffer options
    use "windwp/nvim-autopairs" -- Auto insert pairs like () [] {}
    use "lewis6991/gitsigns.nvim" -- Signs next to line numbers to show git status of a line
    use "tpope/vim-fugitive" -- Best Git Client after magit :)
    use "fatih/vim-go" -- Golang tools and code actions
    use "akinsho/toggleterm.nvim" -- Terminal emulator that we deserve
    use "dag/vim-fish"
  end,
  config = {
    compile_path = vim.fn.stdpath "data" .. "/site/plugin/packer_compiled.lua",
    display = {
      open_fn = function()
        return require("packer.util").float { border = "rounded" }
      end,
    },
  },
}

-- Simple function to reduce boilerplate
-- when configuring a plugin using a conventional
-- Lua interface.
local function setup(plugin, opts)
  local has_plugin, _ = pcall(require, plugin)
  if has_plugin then
    require(plugin).setup(opts)
  end
end

-- Install missing plugins
require("packer").install()

-- Faster lua module lookup by caching
pcall(require, "impatient")

-- Colorscheme
setup("rose-pine", {
  disable_background = vim.g.transparent,
  disable_float_background = vim.g.transparent,
})

setup("tokyonight", {
  transparent = vim.g.transparent,
})

setup("catppuccin", {
  transparent_background = vim.g.transparent,
})

setup("gruvbox", {
  transparent_mode = vim.g.transparent,
})

pcall(vim.cmd.colorscheme, "tokyonight-night")

-- File manager like a boss
setup("oil", {})

if if_has "mason" then
  setup("mason", {})
  for _, pkg in ipairs { "stylua", "golangci-lint", "goimports", "gofumpt", "yamlfmt" } do -- ensure these tools are installed
    if not require("mason-registry").is_installed(pkg) then
      require("mason.api.command").MasonInstall { pkg }
    end
  end
end

if if_has("mason", "lspconfig", "mason_lspconfig", "cmp_nvim_lsp") then
  local mason_lspconfig = require "mason-lspconfig"
  local ensure_installed = {
    gopls = {},
    lua_ls = {
      Lua = {
        telemetry = { enable = false },
        diagnostics = {
          globals = { "vim" },
        },
        workspace = {
          checkThirdParty = false,
          library = vim.api.nvim_get_runtime_file("", true),
        },
      },
    },
    rust_analyzer = {},
    zls = {},
  }
  mason_lspconfig.setup {
    ensure_installed = vim.tbl_keys(ensure_installed),
  }

  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

  mason_lspconfig.setup_handlers {
    function(server_name)
      require("lspconfig")[server_name].setup {
        capabilities = capabilities,
        on_attach = function(_, bufnr)
          vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
          local buffer = { buffer = bufnr }
          vim.keymap.set("n", "gd", vim.lsp.buf.definition, buffer)
          vim.keymap.set("n", "gD", vim.lsp.buf.declaration, buffer)
          vim.keymap.set("n", "gi", vim.lsp.buf.implementation, buffer)
          vim.keymap.set("n", "gr", vim.lsp.buf.references, buffer)
          vim.keymap.set("n", "R", vim.lsp.buf.rename, buffer)
          vim.keymap.set("n", "<F2>", vim.lsp.buf.rename, buffer)
          vim.keymap.set("n", "K", vim.lsp.buf.hover, buffer)
          vim.keymap.set("n", "gf", vim.lsp.buf.format, buffer)

          vim.keymap.set("n", "gl", vim.diagnostic.open_float, buffer)
          vim.keymap.set("n", "gp", vim.diagnostic.goto_prev, buffer)
          vim.keymap.set("n", "gn", vim.diagnostic.goto_next, buffer)

          vim.keymap.set("n", "C", vim.lsp.buf.code_action, buffer)
          vim.keymap.set("n", "<C-s>", vim.lsp.buf.signature_help, buffer)
          vim.keymap.set("i", "<C-s>", vim.lsp.buf.signature_help, buffer)
        end,

        settings = ensure_installed[server_name],
        handlers = {
          ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" }),
          ["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" }),
        },
      }
    end,
  }
  local autoformat_patterns = {
    "*.rs",
    "*.lua",
  }

  vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = autoformat_patterns,
    callback = function(_)
      vim.lsp.buf.format()
    end,
  })
  local virtual_text = true
  vim.api.nvim_create_user_command("VirtualTextToggle", function()
    virtual_text = not virtual_text
    vim.diagnostic.config {
      virtual_text = virtual_text,
    }
  end, {})

  vim.diagnostic.config {
    virtual_text = true,
  }
end

-- Hook non-LSP sources into LSP
setup("null-ls", {
  sources = {
    require("null-ls").builtins.code_actions.gitsigns,

    require("null-ls").builtins.diagnostics.golangci_lint,
    require("null-ls").builtins.diagnostics.trail_space.with { disabled_filetypes = { "NvimTree" } },

    require("null-ls").builtins.formatting.stylua,
    require("null-ls").builtins.formatting.goimports,
  },
})

if if_has("cmp", "luasnip") then
  require("luasnip").config.setup {}

  local luasnip = require "luasnip"

  local cmp = require "cmp"

  cmp.setup {
    snippet = {
      expand = function(args)
        luasnip.lsp_expand(args.body)
      end,
    },
    mapping = cmp.mapping.preset.insert {
      ["<C-d>"] = cmp.mapping.scroll_docs(-4),
      ["<C-f>"] = cmp.mapping.scroll_docs(4),
      ["<C-Space>"] = cmp.mapping.complete {},
      ["<CR>"] = cmp.mapping.confirm {
        behavior = cmp.ConfirmBehavior.Replace,
        select = true,
      },
      ["<Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
        elseif luasnip.expand_or_jumpable() then
          luasnip.expand_or_jump()
        else
          fallback()
        end
      end, { "i", "s" }),
      ["<S-Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        elseif luasnip.jumpable(-1) then
          luasnip.jump(-1)
        else
          fallback()
        end
      end, { "i", "s" }),
    },
    sources = {
      { name = "nvim_lsp" },
      { name = "luasnip" },
    },
  }
end

-- Some Golang stuff
vim.g.go_gopls_enabled = 0
vim.g.go_template_autocreate = 0
local go_group = vim.api.nvim_create_augroup("go", {})
vim.api.nvim_create_autocmd("BufEnter", {
  pattern = "*.go",
  group = go_group,
  callback = function(meta)
    local buffer = { buffer = meta.bufnr, remap = true }
    vim.keymap.set("n", "<leader>ma", "<cmd>GoAddTag<CR>", buffer)
    vim.keymap.set("n", "<leader>md", "<cmd>GoRmTag<CR>", buffer)
    vim.keymap.set("n", "<leader>mf", "<cmd>GoFillStruct<CR>", buffer)
  end,
})

-- Terminal emulator
setup("toggleterm", {
  size = function(term)
    if term.direction == "horizontal" then
      return 15
    elseif term.direction == "vertical" then
      return vim.o.columns * 0.4
    end
  end,
  direction = "vertical",
})

vim.keymap.set({ "n", "t" }, "<C-`>", "<cmd>ToggleTerm<CR>", {})

setup("nvim-treesitter.configs", {
  ensure_installed = { "json", "yaml", "c", "cpp", "lua", "rust", "go", "python", "php" },
  context_commentstring = {
    enable = true,
  },
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
})

setup("Comment", {})

setup("gitsigns", {
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "_" },
    topdelete = { text = "‾" },
    changedelete = { text = "~" },
  },
})

vim.keymap.set("n", "<leader>gb", function()
  vim.cmd.Gitsigns "blame_line"
end)

vim.keymap.set("n", "<leader>gs", vim.cmd.Git)

vim.api.nvim_create_user_command("Gp", function()
  vim.cmd.Git "push"
end, {})

if if_has "telescope" then
  local no_preview = { previewer = false }

  require("telescope").setup {}
  require("telescope").load_extension "fzf"

  local function telescope_smart_find_files(opts)
    local git_files = require("telescope.builtin").git_files
    local status, _ = pcall(git_files)
    if not status then
      require("telescope.builtin").find_files(opts)
    end
  end

  vim.keymap.set("n", "<leader><leader>", function()
    telescope_smart_find_files(no_preview)
  end)

  vim.keymap.set("n", "<leader>ff", function()
    require("telescope.builtin").find_files(no_preview)
  end)

  vim.keymap.set("n", "<leader>gf", function()
    require("telescope.builtin").git_files(no_preview)
  end)

  vim.keymap.set("n", "<leader>fs", function()
    require("telescope.builtin").live_grep()
  end)

  vim.keymap.set("n", "??", function()
    require("telescope.builtin").live_grep()
  end)

  vim.keymap.set("n", "<leader>fc", function()
    require("telescope.builtin").commands()
  end)

  vim.keymap.set("n", "<leader>fh", function()
    require("telescope.builtin").help_tags(no_preview)
  end)

  -- Edit configurations
  vim.keymap.set("n", "<leader>fd", function()
    require("telescope.builtin").find_files(vim.tbl_extend("keep", { cwd = "~/dev/dotfiles" }, no_preview))
  end)
end
