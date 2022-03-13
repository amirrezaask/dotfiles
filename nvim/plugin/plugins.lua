require("packer").startup {
  function(use)
    use { "wbthomason/packer.nvim" } -- Plugin manager
    use { "jghauser/mkdir.nvim", config = function() require "mkdir" end } -- Mkdir
    use { "amirrezaask/base16.nvim" } -- Base16 Themes
    use { "dracula/vim" }
    use { "gruvbox-community/gruvbox" }
    use { "tpope/vim-fugitive" } -- Vim Git bindings
    use { "windwp/nvim-spectre", requires = { "nvim-lua/plenary.nvim" } }
    use { "nvim-telescope/telescope.nvim", requires = { "nvim-lua/plenary.nvim" } } -- UI to search for things
    use { "tpope/vim-surround" } -- Vim surround objects
    use { "neovim/nvim-lspconfig" } -- LSP configurations
    use { "honza/dockerfile.vim" } -- Dockerfile
    use { "hashivim/vim-terraform" } -- Terraform
    use { "LnL7/vim-nix" } -- Nix
    use { "dag/vim-fish" } -- Fish
    use { "cespare/vim-toml" } -- Toml
    use { "elixir-editors/vim-elixir" } -- Elixir
    use { "pearofducks/ansible-vim" } -- Ansible
    use { "Glench/Vim-Jinja2-Syntax" } -- Jinja2
    use { "amirrezaask/actions.nvim" } -- Define IDE like actions.
    use { "purescript-contrib/purescript-vim" }
    use { "ziglang/zig.vim" } -- Zig language Support
    use { "hrsh7th/nvim-cmp" } -- completion popup
    use { "hrsh7th/cmp-buffer" } -- source for completion from words in current buffer
    use { "hrsh7th/cmp-nvim-lua" } -- source for completion from neovim stuff
    use { "hrsh7th/cmp-nvim-lsp" } -- source for completion from lsp
    use { "hrsh7th/cmp-path" } -- source for completion from fs path
    use { "rust-lang/rust.vim", ft = "rust" } -- rust syntax
    use { "nvim-treesitter/nvim-treesitter" } -- treesitter integration
    use { "nvim-treesitter/nvim-treesitter-textobjects" } -- more text objects for treesitter
    use { "godlygeek/tabular" } -- Beautify text
    use { "lukas-reineke/indent-blankline.nvim" } -- Show indent highlights
    use { "fatih/vim-go" } -- Golang IDE
    use { 'kyazdani42/nvim-web-devicons' }
    use { 'yamatsum/nvim-web-nonicons' }
    use { "j-hui/fidget.nvim" }
    use { "rcarriga/nvim-notify" }
    use { "numToStr/Comment.nvim" }
    use { "L3MON4D3/LuaSnip" }
    use { 'fladson/vim-kitty' }
    use { 'vim-erlang/vim-erlang-runtime' }
    use { 'cohama/agit.vim' }
    use {
      'nvim-lualine/lualine.nvim',
      requires = { 'kyazdani42/nvim-web-devicons', opt = true }
    }
    use { 'mhinz/vim-startify' }
    use { 'ful1e5/onedark.nvim' }
    use { 'folke/tokyonight.nvim' }
    use { 'shaunsingh/nord.nvim' }
    use { 'mfussenegger/nvim-dap' }
    use { 'junegunn/fzf' }
    use { 'junegunn/fzf.vim' }
    use { 'tanvirtin/monokai.nvim' }
    use { 'eemed/sitruuna.vim' }
  end,
}


