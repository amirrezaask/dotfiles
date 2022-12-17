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

local packer_bootstrap = ensure_packer()

-- Initialize packer
require("packer").init {
  compile_path = vim.fn.stdpath "data" .. "/site/plugin/packer_compiled.lua",
  display = {
    open_fn = function()
      return require("packer.util").float { border = "rounded" }
    end,
  },
}

local _configs = {}

local use = function(spec)
  if type(spec) == "table" then
    if spec.config then
      table.insert(_configs, spec.config)
      spec.config = nil
    end
  elseif type(spec) == "function" then
    table.insert(_configs, spec)
    return
  end
  require("packer").use(spec)
end

use "wbthomason/packer.nvim"
use "lewis6991/impatient.nvim"
use "NTBBloodbath/doom-one.nvim"
use "navarasu/onedark.nvim"
use {
  "folke/tokyonight.nvim",
  config = function()
    require("tokyonight").setup {
      style = "night",
    }
  end,
}

use { "rose-pine/neovim", as = "rose-pine" }
use { "catppuccin/nvim", as = "catppuccin" }

-- Set the colorscheme
use(function()
  pcall(function()
    vim.cmd.colorscheme "catppuccin-mocha"
  end)
end)

-- Comment
use {
  "numToStr/Comment.nvim",
  config = function()
    require("Comment").setup()
  end,
}

-- Statusline
use {
  "nvim-lualine/lualine.nvim",
  config = function()
    require("lualine").setup {
      options = {
        icons_enabled = false,
        theme = "auto",
        component_separators = "|",
        section_separators = "",
      },
    }
  end,
}

-- Indent blanklines
use {
  "lukas-reineke/indent-blankline.nvim",
}

-- Telescope
use {
  "nvim-telescope/telescope.nvim",
  requires = {
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      run = "make",
    },
    { "nvim-lua/plenary.nvim" },
  },

  config = function()
    require "user.telescope"
  end,
}

-- Treesitter
use {
  "nvim-treesitter/nvim-treesitter",
  requires = {
    "nvim-treesitter/nvim-treesitter-textobjects",
    "p00f/nvim-ts-rainbow",
    "JoosepAlviste/nvim-ts-context-commentstring",
    "nvim-treesitter/playground",
  },
  config = function()
    require "user.treesitter"
  end,
}

use {
  "nvim-treesitter/nvim-treesitter-context",
  config = function()
    require("treesitter-context").setup {}
  end,
}

use {
  "VonHeikemen/lsp-zero.nvim",
  requires = {
    -- LSP Support
    { "neovim/nvim-lspconfig" },
    { "williamboman/mason.nvim" },
    { "williamboman/mason-lspconfig.nvim" },

    -- Autocompletion
    { "hrsh7th/nvim-cmp" },
    { "hrsh7th/cmp-buffer" },
    { "hrsh7th/cmp-path" },
    { "saadparwaiz1/cmp_luasnip" },
    { "hrsh7th/cmp-nvim-lsp" },
    { "hrsh7th/cmp-nvim-lua" },

    -- Snippets
    { "L3MON4D3/LuaSnip" },
    { "rafamadriz/friendly-snippets" },

    -- Json langauge server schemas
    { "b0o/schemastore.nvim" },

    -- Null ls
    { "jose-elias-alvarez/null-ls.nvim" },

    -- Fidget , standalone UI for lsp progress
    { "j-hui/fidget.nvim" },
  },
  config = function()
    require "user.lsp"
  end,
}

-- Automatically create directory when you create a new file in a directory that
-- does not exists.
use "pbrisbin/vim-mkdir"

-- Support for many filetypes.
use "sheerun/vim-polyglot"

-- Support Kitty terminal config syntax
use "fladson/vim-kitty"

-- Support for helm template syntax
use "towolf/vim-helm"

-- surrounding text objects
use "tpope/vim-surround"

-- Preview quickfix list item.
use "kevinhwang91/nvim-bqf"

-- Helper commands like :Rename, :Move, :Delete, :Remove, ...
use "tpope/vim-eunuch"

-- Heuristically set buffer options
use "tpope/vim-sleuth"

use {
  "windwp/nvim-autopairs",
  config = function()
    require("nvim-autopairs").setup()
  end,
}

-- Better UI for neovim default UI components like input box or selecting
use {
  "stevearc/dressing.nvim",
  config = function()
    require("dressing").setup {
      input = {
        enabled = true,
      },
      select = {
        enabled = true,
      },
    }
  end,
}

-- Git stuff
use {
  "lewis6991/gitsigns.nvim",
  config = function()
    require("gitsigns").setup {
      signs = {
        add = { text = "+" },
        change = { text = "~" },
        delete = { text = "_" },
        topdelete = { text = "‾" },
        changedelete = { text = "~" },
      },
    }
  end,
}

use {
  "tpope/vim-fugitive",
  config = function()
    vim.keymap.set("n", "<leader>gs", vim.cmd.Git)
    vim.api.nvim_create_user_command("Gp", function()
      vim.cmd.Git "push"
    end, {})
  end,
}

-- Golang tools and code actions
use {
  "fatih/vim-go",
  config = function()
    vim.g.go_gopls_enabled = 0
    vim.g.go_template_autocreate = 0
    local go_group = vim.api.nvim_create_augroup("go", {})
    vim.api.nvim_create_autocmd("BufEnter", {
      pattern = "*.go",
      group = go_group,
      callback = function(meta)
        local buffer = { buffer = meta.bufnr, remap = true }
        vim.keymap.set("n", "<leader>lat", "<cmd>GoAddTag<CR>", buffer)
        vim.keymap.set("n", "<leader>lrt", "<cmd>GoRmTag<CR>", buffer)
        vim.keymap.set("n", "<leader>lfs", "<cmd>GoFillStruct<CR>", buffer)
      end,
    })
  end,
}

use {
  "akinsho/toggleterm.nvim",
  config = function()
    require("toggleterm").setup {
      size = function(term)
        if term.direction == "horizontal" then
          return 15
        elseif term.direction == "vertical" then
          return vim.o.columns * 0.4
        end
      end,
      direction = "vertical",
    }

    vim.keymap.set({ "n", "t" }, "<C-`>", "<cmd>ToggleTerm<CR>", {})
  end,
}

use {
  "folke/zen-mode.nvim",
  config = function()
    require("zen-mode").setup {}
    vim.keymap.set("n", "<leader>z", vim.cmd.ZenMode)
  end,
}

if packer_bootstrap then
  require("packer").sync()
end

require("packer").install()
pcall(require, "impatient")

for _, cfg in ipairs(_configs) do
  cfg()
end
