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

require("packer").startup {
  function(use)
    use "wbthomason/packer.nvim"
    use "lewis6991/impatient.nvim"

    use "numToStr/Comment.nvim"

    use "sainnhe/sonokai"
    use "folke/tokyonight.nvim"
    use "Mofiqul/dracula.nvim"
    use "ellisonleao/gruvbox.nvim"
    use "bluz71/vim-nightfly-colors"
    use "navarasu/onedark.nvim"
    use "Shatur/neovim-ayu"
    use "rose-pine/neovim"
    use "EdenEast/nightfox.nvim"
    use "bluz71/vim-moonfly-colors"
    use { "catppuccin/nvim", as = "catppuccin" }

    -- Telescope
    use {
      "nvim-telescope/telescope.nvim",
      requires = { { "nvim-lua/plenary.nvim" } },
    }

    use {
      "nvim-telescope/telescope-fzf-native.nvim",
      run = "make",
    }

    -- Statusline
    -- use {
    --   "nvim-lualine/lualine.nvim",
    --   requires = { "kyazdani42/nvim-web-devicons", opt = true },
    -- }
    -- Treesitter
    use {
      "nvim-treesitter/nvim-treesitter",
      requires = {
        "nvim-treesitter/nvim-treesitter-textobjects",
        "p00f/nvim-ts-rainbow",
        "nvim-treesitter/nvim-treesitter-context",
      },
    }

    -- LSP Zero
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
      },
    }

    -- Automatically create directory when you create a new file in a directory that
    -- does not exists.
    use "pbrisbin/vim-mkdir"

    -- Support for many filetypes.
    use "sheerun/vim-polyglot"

    -- toggle a window to be maximized, like tmux zoom
    use "szw/vim-maximizer"

    -- Support Kitty terminal syntax
    use "fladson/vim-kitty"

    use "towolf/vim-helm"

    use "tpope/vim-surround"

    use "junegunn/vim-easy-align"

    -- Preview quickfix list item.
    use "kevinhwang91/nvim-bqf"

    -- Helper commands like :Rename, :Move, :Delete, :Remove, ...
    use "tpope/vim-eunuch"

    -- Heuristically set buffer options
    use "tpope/vim-sleuth"

    use {
      "windwp/nvim-autopairs",
    }

    use {
      "stevearc/dressing.nvim",
    }

    use {
      "lewis6991/gitsigns.nvim",
    }

    use {
      "junegunn/gv.vim",
    }

    use {
      "cohama/agit.vim",
    }

    use {
      "tpope/vim-fugitive",
    }

    -- Golang tools and code actions
    use {
      "fatih/vim-go",
    }

    use {
      "ThePrimeagen/harpoon",
      requires = {
        "nvim-lua/plenary.nvim",
      },
    }

    use "ThePrimeagen/vim-be-good"

    use {
      "b0o/schemastore.nvim",
    }

    use "milisims/nvim-luaref"
    use "nanotee/luv-vimdocs"

    -- Hook non LSP tools into neovim LSP client
    use {
      "jose-elias-alvarez/null-ls.nvim",
    }

    use "Glench/Vim-Jinja2-Syntax"

    use {
      "rust-lang/rust.vim",
    }

    use {
      "simrat39/rust-tools.nvim",
    }

    -- Tmux integration, look here for tmux part https://github.com/mrjones2014/smart-splits.nvim#tmux-integration
    use {
      "mrjones2014/smart-splits.nvim",
    }

    use {
      "ziglang/zig.vim",
    }
    use {
      "akinsho/toggleterm.nvim",
    }
    use {
      "nvim-tree/nvim-tree.lua",
    }
  end,
  config = {
    compile_path = require("packer.util").join_paths(vim.fn.stdpath "data", "packer_compiled.lua"),
  },
}

if packer_bootstrap then
  require("packer").sync()
end

pcall(require, "impatient")