return {
    {
        "rose-pine/neovim",
        name = "rose-pine",
        config = function()
            require "rose-pine".setup({
                styles = {
                    italic = false,
                    transparency = TRANSPARENT
                }
            })
            -- vim.cmd.colorscheme("rose-pine")
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
        config = function()
            require"tokyonight".setup({
                transparent = TRANSPARENT,
            })

            vim.cmd.colorscheme("tokyonight-night")
        end,
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
