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

-- Now we install plugins

require("packer").startup {
  function(use)
    use "wbthomason/packer.nvim"

    -- Colorschemes
    use "eemed/sitruuna.vim"
    use "sainnhe/sonokai"
    use "folke/tokyonight.nvim"
    use "ellisonleao/gruvbox.nvim"
    use "bluz71/vim-nightfly-colors"

    use "junegunn/goyo.vim"

    -- UI enhancements
    use "nvim-tree/nvim-web-devicons"
    use "yamatsum/nvim-nonicons"
    use {
      "stevearc/dressing.nvim",
    }
    use {
      "folke/which-key.nvim",
    }

    -- LSP
    use {
      "j-hui/fidget.nvim",
    }
    use {
      "williamboman/mason.nvim",
    }
    use "williamboman/mason-lspconfig.nvim"
    use "jayp0521/mason-nvim-dap.nvim"

    use "neovim/nvim-lspconfig"
    use {
      "ray-x/lsp_signature.nvim",
    }
    use {
      "glepnir/lspsaga.nvim",
      branch = "main",
    }
    use "onsails/lspkind.nvim"

    -- JSON Scheumas
    use "b0o/schemastore.nvim"

    -- Git stuff
    use "tpope/vim-fugitive"
    use "junegunn/gv.vim"
    use "cohama/agit.vim"
    use {
      "lewis6991/gitsigns.nvim",
    }
    use {
      "TimUntersberger/neogit",
      requires = "nvim-lua/plenary.nvim",
    }

    -- Snippets
    use { "L3MON4D3/LuaSnip", tag = "v<CurrentMajor>.*", requires = "rafamadriz/friendly-snippets" }

    -- Completion
    use {
      "hrsh7th/nvim-cmp",
      requires = {
        "saadparwaiz1/cmp_luasnip",
        "hrsh7th/cmp-buffer",
        "hrsh7th/cmp-nvim-lua",
        "hrsh7th/cmp-nvim-lsp",
        "hrsh7th/cmp-path",
      },
    }

    -- Treesitter
    use {
      "nvim-treesitter/nvim-treesitter",
      requires = {
        "nvim-treesitter/nvim-treesitter-textobjects",
        "p00f/nvim-ts-rainbow",
      },
    }

    -- Telescope
    use {
      "nvim-telescope/telescope.nvim",
      requires = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope-file-browser.nvim" },
    }
    use {
      "nvim-telescope/telescope-fzf-native.nvim",
      run = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
    }

    use "sheerun/vim-polyglot"

    use "towolf/vim-helm"

    use "pbrisbin/vim-mkdir"

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

    -- Rust
    use "rust-lang/rust.vim"
    use "simrat39/rust-tools.nvim"

    use "folke/neodev.nvim"

    use "cuducos/yaml.nvim"

    use "Glench/Vim-Jinja2-Syntax"

    -- statusline
    use "nvim-lualine/lualine.nvim"

    -- more convinient terminal emulator
    use {
      "akinsho/toggleterm.nvim",
      tag = "*",
    }

    -- dashboard or start screen
    use {
      "goolord/alpha-nvim",
      requires = { "kyazdani42/nvim-web-devicons" },
    }

    -- Commenting plugin
    use {
      "numToStr/Comment.nvim",
    }

    -- Integrate Tmux and neovim pane and window switching
    use "mrjones2014/smart-splits.nvim"
    use "christoomey/vim-tmux-navigator"

    -- Toggle window zoom like Tmux
    use "szw/vim-maximizer"
    use {
      "nvim-tree/nvim-tree.lua",
      requires = {
        "nvim-tree/nvim-web-devicons", -- optional, for file icons
      },
      tag = "nightly", -- optional, updated every week. (see issue #1193)
    }
    use "windwp/nvim-autopairs"
  end,
  config = {
    display = {
      open_fn = require("packer.util").float,
    },
  },
}
