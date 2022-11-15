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

  use "j-hui/fidget.nvim"
  use "junegunn/goyo.vim"
  use "mhinz/vim-startify"
  use "nvim-tree/nvim-web-devicons"
  use "yamatsum/nvim-nonicons"
  use "stevearc/dressing.nvim"
  use "folke/which-key.nvim"

  -- LSP
  use "neovim/nvim-lspconfig"
  use "williamboman/mason.nvim"

  -- Git stuff
  use "tpope/vim-fugitive"
  use "junegunn/gv.vim"
  use "cohama/agit.vim"
  use "lewis6991/gitsigns.nvim"
  use { "TimUntersberger/neogit", requires = "nvim-lua/plenary.nvim" }

  -- Completion
  use "hrsh7th/vim-vsnip" -- Snippets
  use "hrsh7th/nvim-cmp" -- Neovim auto complete menu
  use "hrsh7th/cmp-buffer" -- auto complete buffer source
  use "hrsh7th/cmp-nvim-lua" -- auto complete nvim lua stuff source
  use "hrsh7th/cmp-nvim-lsp" -- auto complete lsp source
  use "hrsh7th/cmp-path" -- auto complete os path source

  -- Treesitter
  use "nvim-treesitter/nvim-treesitter"
  use "nvim-treesitter/nvim-treesitter-textobjects"
  use "p00f/nvim-ts-rainbow"

  -- Navigation and fuzzy finder
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
  }

  use "ziglang/zig.vim"
  use "jansedivy/jai.vim"

  use { "ckipp01/stylua-nvim", run = "cargo install stylua" }
  use "rust-lang/rust.vim"
  use "simrat39/rust-tools.nvim"

  use "cuducos/yaml.nvim"

  use "Glench/Vim-Jinja2-Syntax"
  use 'nvim-lualine/lualine.nvim'
end)
