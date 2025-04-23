local function config(name)
  return function()
    require("plugins." .. name)
  end
end

return {
  -- Colorschemes [[
  { "rose-pine/neovim", enabled = false, name = "rose-pine" },
  { "catppuccin/nvim", enabled = false, name = "catppuccin" },
  { "folke/tokyonight.nvim", enabled = false, name = "tokyonight" },
  { "embark-theme/vim", enabled = false, name = "embark" },
  { "dracula/vim", enabled = false, name = "dracula" },
  { "ellisonleao/gruvbox.nvim", enabled = false, name = "gruvbox" },
  { "navarasu/onedark.nvim", enabled = false, name = "onedark" },
  { "shaunsingh/nord.nvim", enabled = false, name = "nord" },
  { "sainnhe/edge", enabled = false, name = "edge" },
  { "sainnhe/everforest", enabled = false, name = "everforest" },
  { "sainnhe/sonokai", enabled = false, name = "sonokai" },
  { "sainnhe/gruvbox-material", enabled = false, name = "gruvbox-material" },
  { "oxfist/night-owl.nvim", enabled = false, name = "night-owl" },
  { "miikanissi/modus-themes.nvim", enabled = false, name = "modus-themes" },
  { "scottmckendry/cyberdream.nvim", enabled = false, name = "cyberdream" },
  { "samharju/serene.nvim", enabled = false, name = "serene" },
  -- ]]

  { -- Help with neovim/lua dev.
    "folke/lazydev.nvim",
    ft = "lua",
    cmd = "LazyDev",
    opts = {
      library = {
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
  },

  { -- Blazingly fast autocomplete
    "saghen/blink.cmp",
    tag = "v1.1.1",
    config = config("blinkcmp"),
  },

  { -- Autoformat/fixes
    "stevearc/conform.nvim",
    config = config("conform"),
  },

  -- LSP progression status
  { "j-hui/fidget.nvim", opts = {} },

  { -- Package manager for your system inside neovim.
    "williamboman/mason.nvim",
    opts = {},
  },

  { "kevinhwang91/nvim-bqf", opts = {} },

  {
    "ibhagwan/fzf-lua",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
      { "junegunn/fzf", build = "./install --all" }, -- This is not really a dependency, it just makes sure that fzf is laready installed into my system.
    },
    config = config("fzf"),
  },

  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
    config = config("nvim-treesitter"),
  },

  {
    "stevearc/oil.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = config("oil"),
  },

  { "lewis6991/gitsigns.nvim", config = config("gitsigns") },

  { "folke/snacks.nvim", enabled = false, config = config("snacks") },

  { -- AI Apocalypse
    "supermaven-inc/supermaven-nvim",
    opts = {},
  },

  { -- Find/Replace project wide.
    "MagicDuck/grug-far.nvim",
    config = config("grug-far"),
  },

  { -- Legendary fuzzy finder
    "nvim-telescope/telescope.nvim",
    enabled = false,
    dependencies = {
      { "nvim-tree/nvim-web-devicons" },
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
      { "nvim-lua/plenary.nvim" },
      { "nvim-telescope/telescope-ui-select.nvim" },
    },
    config = config("telescope"),
  },
}
