return {
    'amirrezaask/nvim-blue.lua',
    'amirrezaask/nvim-gruvbuddy.lua',
    { 'rose-pine/neovim', name = 'rose-pine' },
    { 'catppuccin/nvim',  name = 'catppuccin' },
    'folke/tokyonight.nvim',

    {
        "saghen/blink.cmp",
        tag = "v1.1.1",
        config = require("custom.blinkcmp")
    },

    {
        "williamboman/mason.nvim",
        opts = {}
    },

    {
        "ibhagwan/fzf-lua",
        dependencies = {
            'nvim-tree/nvim-web-devicons',
            { "junegunn/fzf", build = "./install --all" }, -- This is not really a dependency, it just makes sure that fzf is laready installed into my system.
        },
        config = require("custom.fzf"),
    },


    {
        'amirrezaask/nvim-terminal.lua',
        config = require("custom.nvim-terminal")
    },

    {
        "nvim-treesitter/nvim-treesitter",
        config = require("custom.nvim-treesitter")
    },
    {
        "stevearc/oil.nvim",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        config = require("custom.oil"),
    },

}
