return {    -- Treesitter
    {
        "nvim-treesitter/nvim-treesitter",
        dependencies = {
            "nvim-treesitter/nvim-treesitter-textobjects",
            "nvim-treesitter/playground",
        },
        config = function()
            require("nvim-treesitter.configs").setup({
                ensure_installed = {},
                sync_install = false,
                auto_install = true,
                ignore_install = {},
                modules = {},
                context_commentstring = { enable = true },
                highlight = { enable = true, additional_vim_regex_highlighting = false },
                textobjects = {
                    select = {
                        enable = true,
                        lookahead = true,
                        keymaps = {
                            ["af"] = "@function.outer",
                            ["if"] = "@function.inner",
                            ["ac"] = "@class.outer",
                            ["ic"] = "@class.inner",
                        },
                    },
                },
            })
        end,
    }
}
