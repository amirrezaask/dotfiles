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
}
