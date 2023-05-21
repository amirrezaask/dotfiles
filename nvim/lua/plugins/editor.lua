return {
    "kevinhwang91/nvim-bqf", -- Preview quickfix list item.
    "tpope/vim-surround", -- surrounding text objects
    "windwp/nvim-autopairs", -- Auto insert pairs like () [] {}
    {
        "folke/zen-mode.nvim",
        config = function() require("zen-mode").setup() end,
    },
    {
        "folke/which-key.nvim",
        config = function()
            require("which-key").setup {
                window = {
                    border = "single",
                },
            }
        end,
    }, -- Cheat your way through keymapings
    {
        "numToStr/Comment.nvim",
        config = function() require("Comment").setup() end,
    },
}
