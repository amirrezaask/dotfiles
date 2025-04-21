local function config(name)
    return function()
        require("plugins." .. name)
    end
end

return {
    -- Colorschemes [[
    { 'rose-pine/neovim',              name = 'rose-pine' },
    { 'catppuccin/nvim',               name = 'catppuccin' },
    { 'folke/tokyonight.nvim',         name = 'tokyonight' },
    { 'embark-theme/vim',              name = 'embark' },
    { 'dracula/vim',                   name = 'dracula' },
    { 'ellisonleao/gruvbox.nvim',      name = 'gruvbox' },
    { 'navarasu/onedark.nvim',         name = 'onedark' },
    { 'shaunsingh/nord.nvim',          name = 'nord' },
    { 'sainnhe/edge',                  name = 'edge' },
    { 'sainnhe/everforest',            name = 'everforest' },
    { 'sainnhe/sonokai',               name = 'sonokai' },
    { 'sainnhe/gruvbox-material',      name = 'gruvbox-material' },
    { "oxfist/night-owl.nvim",         name = 'night-owl' },
    { "miikanissi/modus-themes.nvim",  name = 'modus-themes' },
    { "scottmckendry/cyberdream.nvim", name = 'cyberdream' },
    { "samharju/serene.nvim",          name = 'serene' },
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
        "saghen/blink.cmp", tag = "v1.1.1", config = config("blinkcmp")
    },

    { -- Autoformat/fixes
        'stevearc/conform.nvim', config = config("conform"),
    },

    { "j-hui/fidget.nvim",     opts = {} },

    { -- Package manager for your system inside neovim.
        "williamboman/mason.nvim", opts = {}
    },

    { 'kevinhwang91/nvim-bqf', opts = {} },

    {
        "ibhagwan/fzf-lua",
        dependencies = {
            'nvim-tree/nvim-web-devicons',
            { "junegunn/fzf", build = "./install --all" }, -- This is not really a dependency, it just makes sure that fzf is laready installed into my system.
        },
        config = config("fzf"),
    },

    {
        "nvim-treesitter/nvim-treesitter", config = config("nvim-treesitter")
    },

    {
        "stevearc/oil.nvim", dependencies = { "nvim-tree/nvim-web-devicons" }, config = config("oil"),
    },

    { 'lewis6991/gitsigns.nvim', config = config("gitsigns") },

    { "folke/snacks.nvim",       config = config("snacks") },

    { -- AI Apocalypse
        "supermaven-inc/supermaven-nvim", opts = {},
    },

    { -- Find/Replace project wide.
        'MagicDuck/grug-far.nvim',
        config = config("grug-far"),
    },

    { -- Legendary fuzzy finder
        "nvim-telescope/telescope.nvim",
        dependencies = {
            { 'nvim-tree/nvim-web-devicons' },
            { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
            { "nvim-lua/plenary.nvim" },
            { 'nvim-telescope/telescope-ui-select.nvim' }

        },
        config = config("telescope"),
    },

}
