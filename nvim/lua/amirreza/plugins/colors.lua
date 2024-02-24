return {
    {
        "rose-pine/neovim",
        name = "rose-pine",
        config = function()
            require "rose-pine".setup({
                disable_background = true,
            })
            vim.cmd.colorscheme("rose-pine")
        end,
    },
    {
        "catppuccin/nvim",
        name = "catppuccin",
        opts = {
            no_italic = false,        -- Force no italic
            no_bold = false,          -- Force no bold
            no_underline = false,     -- Force no underline
            transparent_background = TRANSPARENT,
        }
    },
    {
        'folke/tokyonight.nvim',
        opts = {
            transparent = TRANSPARENT,
        }
    },
    {
        "ellisonleao/gruvbox.nvim",
        opts = {
            transparent_mode = TRANSPARENT,
            contrast = 'hard',
            italic = {
                strings = false,
                emphasis = false,
                comments = false,
                operators = false,
                folds = false,
            }
        }
    },
    {     -- Theme inspired by Atom
        'navarasu/onedark.nvim',
        config = function()
            require('onedark').setup {
                transparent = TRANSPARENT,
                style = 'dark',
            }
        end,
    },




}
