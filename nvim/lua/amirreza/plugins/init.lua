return {
    { "numToStr/Comment.nvim", opts = {} },
    { "tpope/vim-sleuth" },                     -- set buffer options heuristically

    {                                           -- Highlight TODOs in code
        "folke/todo-comments.nvim",
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = {
            signs = false
        }
    },


    {
        "folke/which-key.nvim",
        event = "VeryLazy",
        init = function()
            vim.o.timeout = true
            vim.o.timeoutlen = 300
        end,
        opts = {
            -- your configuration comes here
            -- or leave it empty to use the default settings
            -- refer to the configuration section below
        }
    },


}
