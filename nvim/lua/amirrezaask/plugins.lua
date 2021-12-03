local install_path = vim.fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
local is_wsl = (function()
  return string.find(vim.fn.systemlist("uname -r")[1] or "", "WSL")
end)()
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
end

require("packer").startup {
  function(use)
    use { "wbthomason/packer.nvim" } -- Plugin manager
    use { "navarasu/onedark.nvim" } -- Onedark from Atom
    use { "folke/tokyonight.nvim" } -- Tokyonight
    use { "dracula/vim" } -- Dracula
    use { "gruvbox-community/gruvbox" } -- Gruvbox
    use {
      "nvim-lualine/lualine.nvim",
      config = function()
        require("lualine").setup { options = { theme = "gruvbox" } }
      end,
    }
    use {
      "jghauser/mkdir.nvim",
      config = function()
        require "mkdir"
      end,
    }
    use { "saadparwaiz1/cmp_luasnip" }
    use { "L3MON4D3/LuaSnip" } -- Snippets plugin
    use { "tpope/vim-fugitive" }
    use { "windwp/nvim-spectre", requires = { "nvim-lua/plenary.nvim" } }
    use { "p00f/nvim-ts-rainbow" } -- rainbow parens
    use { "nvim-telescope/telescope.nvim", requires = { "nvim-lua/plenary.nvim" } } -- UI to search for things
    use { "tpope/vim-surround" } -- Vim surround objects
    use { "lewis6991/gitsigns.nvim", requires = { "nvim-lua/plenary.nvim" } } -- Gitsigns
    use { "tpope/vim-commentary" } -- Comment codes at ease
    use { "neovim/nvim-lspconfig" } -- LSP configurations
    use { "norcalli/nvim-colorizer.lua", branch = "color-editor" }
    use { "honza/dockerfile.vim" } -- Dockerfile
    use { "hashivim/vim-terraform" } -- Terraform
    use { "LnL7/vim-nix" } -- Nix
    use { "dag/vim-fish" } -- Fish
    use { "cespare/vim-toml" } -- Toml
    use { "elixir-editors/vim-elixir" } -- Elixir
    use { "pearofducks/ansible-vim" } -- Ansible
    use { "Glench/Vim-Jinja2-Syntax" } -- Jinja2
    use { "amirrezaask/actions.nvim" } -- Define IDE like actions.
    use { "hrsh7th/nvim-cmp" } -- completion popup
    use { "hrsh7th/cmp-buffer" } -- source for completion from words in current buffer
    use { "hrsh7th/cmp-nvim-lua" } -- source for completion from neovim stuff
    use { "hrsh7th/cmp-nvim-lsp" } -- source for completion from lsp
    use { "hrsh7th/cmp-path" } -- source for completion from fs path
    use { "rust-lang/rust.vim", ft = "rust" } -- rust syntax
    use { "nvim-treesitter/nvim-treesitter" } -- treesitter integration
    use { "nvim-treesitter/nvim-treesitter-textobjects" } -- more text objects for treesitter
    use { "mfussenegger/nvim-dap" } -- debug adapter protocol
    use { "theHamsta/nvim-dap-virtual-text" } -- debug adapter protocol virtual text
    use { "kyazdani42/nvim-web-devicons" } -- Icons
    if not is_wsl then
      use { "yamatsum/nvim-nonicons" } -- better Icons
    end
    use { "folke/todo-comments.nvim", requires = "nvim-lua/plenary.nvim" } -- Highlight todo and etc...
    use { "godlygeek/tabular" } -- beautify text
    use { "tjdevries/nlua.nvim" } -- Better lua dev for neovim
    use { "milisims/nvim-luaref" } -- lua reference as vim help
    use { "nanotee/luv-vimdocs" } -- luv reference as vim help
    use { "lukas-reineke/indent-blankline.nvim" }
    use { "ray-x/go.nvim" }
  end,
}
