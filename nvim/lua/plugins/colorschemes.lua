return {
    "amirrezaask/themes",
    {
        "ellisonleao/gruvbox.nvim", -- Best theme of all time
        config = function()
            require("gruvbox").setup {
                transparent_mode = TRANSPARENT,
                contrast = "hard",
                italic = {
                    strings = false,
                    comments = false,
                    operators = false,
                    folds = false,
                },
            }
        end,
    },
    {
        "bluz71/vim-nightfly-colors",
        config = function() vim.nightflyTransparent = true end,
    },
    "shaunsingh/nord.nvim",
    "amirrezaask/sitruuna.vim",
    "shaunsingh/oxocarbon.nvim",
    {
        "rebelot/kanagawa.nvim",
        config = function()
            require("kanagawa").setup {
                transparent = TRANSPARENT,
            }
        end,
    },
    {
        "catppuccin/nvim",
        name = "catppuccin",
        config = function()
            require("catppuccin").setup {
                transparent_background = TRANSPARENT,
            }
        end,
    },
    {
        "rose-pine/neovim",
        name = "rose-pine",
        config = function() require("rose-pine").setup { disable_background = TRANSPARENT } end,
    },
    {
        "folke/tokyonight.nvim",
        config = function() require("tokyonight").setup { transparent = TRANSPARENT } end,
    }, -- folkkkkkeeeeee
}
