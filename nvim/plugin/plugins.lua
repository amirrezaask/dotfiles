require("packer").startup {
  function(use)
    use { "wbthomason/packer.nvim" } -- Plugin manager
    use { "jghauser/mkdir.nvim", config = function() require "mkdir" end } -- Mkdir
    use { "windwp/nvim-spectre", requires = { "nvim-lua/plenary.nvim" } }
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
    use { "ziglang/zig.vim" } -- Zig language Support
    use { "hrsh7th/nvim-cmp" } -- completion popup
    use { "hrsh7th/cmp-buffer" } -- source for completion from words in current buffer
    use { "hrsh7th/cmp-nvim-lua" } -- source for completion from neovim stuff
    use { "hrsh7th/cmp-nvim-lsp" } -- source for completion from lsp
    use { "hrsh7th/cmp-path" } -- source for completion from fs path
    use { "rust-lang/rust.vim", ft = "rust" } -- rust syntax
    use { "nvim-treesitter/nvim-treesitter" } -- treesitter integration
    use { "nvim-treesitter/nvim-treesitter-textobjects" } -- more text objects for treesitter
    use { "lukas-reineke/indent-blankline.nvim" } -- Show indent highlights
    use { "fatih/vim-go" } -- Golang IDE
    use { 'fladson/vim-kitty' } 
    use { 'vim-erlang/vim-erlang-runtime' }
    use { 'junegunn/fzf' }
    use { 'junegunn/fzf.vim' }
    use { 'eemed/sitruuna.vim' }
    use { 'luisiacc/gruvbox-baby' }
    use { 'ap/vim-buftabline' }
    use { 'tpope/vim-commentary' }
  end,
}


