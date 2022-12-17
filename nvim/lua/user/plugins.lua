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
  end
  require("packer").use(spec)
end

use "wbthomason/packer.nvim"
use "lewis6991/impatient.nvim"

local colorscheme = "catppuccin"
local transparent = false

use {
  "folke/tokyonight.nvim",
  config = function()
    require("tokyonight").setup {
      style = "night",
      transparent = transparent,
    }
  end,
}

use "bluz71/vim-nightfly-colors"
use { "rose-pine/neovim", as = "rose-pine" }
use {
  "catppuccin/nvim",
  as = "catppuccin",
  config = function()
    vim.g.catppuccin_flavour = "macchiato"
  end,
}

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
    require("lualine").setup {}
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
    require "user.plugins.telescope"
  end,
}

-- Treesitter
use {
  "nvim-treesitter/nvim-treesitter",
  requires = {
    "nvim-treesitter/nvim-treesitter-textobjects",
    "p00f/nvim-ts-rainbow",
    "nvim-treesitter/nvim-treesitter-context",
    "JoosepAlviste/nvim-ts-context-commentstring",
  },
  config = function()
    require "user.plugins.treesitter"
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
  },
  config = function()
    require "user.plugins.lsp"
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
    require("gitsigns").setup {}
  end,
}

use {
  "tpope/vim-fugitive",
  config = function()
    vim.keymap.set("n", "<leader>gs", "<cmd>Git<cr>")
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
        local nnoremap = vim.keymap.nnoremap
        nnoremap("<leader>lat", "<cmd>GoAddTag<CR>", buffer)
        nnoremap("<leader>lrt", "<cmd>GoRmTag<CR>", buffer)
        nnoremap("<leader>lfs", "<cmd>GoFillStruct<CR>", buffer)
      end,
    })
  end,
}

-- Harpoon
use {
  "ThePrimeagen/harpoon",
  requires = {
    "nvim-lua/plenary.nvim",
  },
  config = function()
    local nnoremap = vim.keymap.nnoremap
    require("telescope").load_extension "harpoon"
    nnoremap("<leader>a", require("harpoon.mark").add_file)
    nnoremap("<leader>1", function()
      require("harpoon.ui").nav_file(1)
    end)
    nnoremap("<leader>2", function()
      require("harpoon.ui").nav_file(2)
    end)
    nnoremap("<leader>3", function()
      require("harpoon.ui").nav_file(3)
    end)
    nnoremap("<leader>4", function()
      require("harpoon.ui").nav_file(4)
    end)
    nnoremap("<leader>5", function()
      require("harpoon.ui").nav_file(5)
    end)
    nnoremap("<leader>6", function()
      require("harpoon.ui").nav_file(6)
    end)
    -- I dont use these two keys so I remap them for harpoon
    nnoremap("L", require("harpoon.ui").nav_next)
    nnoremap("H", require("harpoon.ui").nav_prev)
  end,
}

use "ThePrimeagen/vim-be-good"

use {
  "rust-lang/rust.vim",
}

use {
  "ziglang/zig.vim",
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
    vim.keymap.nnoremap("<leader>z", vim.cmd.ZenMode)
  end,
}

use {
  "folke/which-key.nvim",
  config = function()
    require("which-key").setup()
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

-- Colorscheme of choice
pcall(function()
  vim.cmd("colorscheme " .. colorscheme)
end)
