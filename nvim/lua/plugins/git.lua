return {
    {
        "lewis6991/gitsigns.nvim",
        config = function()
            require("gitsigns").setup {
                signs = {
                    add = { text = "+" },
                    change = { text = "~" },
                    delete = { text = "_" },
                    topdelete = { text = "‾" },
                    changedelete = { text = "~" },
                },
            }
            vim.keymap.set("n", "<leader>b", function() require("gitsigns").blame_line { full = true } end,
                { desc = "Git blame line" })
            vim.keymap.set("n", "<leader>d", function() require("gitsigns").diffthis "~" end,
                { desc = "Diff current file with HEAD" })
        end,
    }, -- Signs next to line numbers to show git status of a line
    {
        "tpope/vim-fugitive",
        config = function()
            vim.api.nvim_create_user_command("Gp", function() vim.cmd.Git "push" end, {})

            vim.keymap.set("n", "<leader>P", function() vim.cmd.Git "push" end, { desc = "Git Push" })
            vim.keymap.set("n", "<leader>g", vim.cmd.Git, { desc = "Git status" })
        end,
    }, -- Second best Git client ( first one is emacs magit )

    {
        "akinsho/git-conflict.nvim",
        version = "*",
        config = function() require("git-conflict").setup {} end,
    },
}
