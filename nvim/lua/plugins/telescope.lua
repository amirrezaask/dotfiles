return {
    -- telescope: Fuzzy finding and searching interface
    "nvim-telescope/telescope.nvim",
    dependencies = {
        "nvim-lua/plenary.nvim",
        { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
        "nvim-telescope/telescope-ui-select.nvim",
    },
    config = function()
        require("telescope").setup {
            extensions = {
                ["ui-select"] = {
                    require("telescope.themes").get_dropdown {},
                },
            },
        } -- Best fuzzy finder
        require("telescope").load_extension "fzf" -- load fzf awesomnes into Telescope
        require("telescope").load_extension "ui-select" -- Use telescope for vim.ui.select
        local no_preview = { previewer = false, layout_config = { height = 0.8 } }
        -- local dropdown = require("telescope.themes").get_dropdown
        local dropdown = function(opts) return opts end
        local telescope_builtin = require "telescope.builtin"
        vim.keymap.set("n", "<C-p>", function() telescope_builtin.git_files(dropdown(no_preview)) end,
            { desc = "Telescope Git Files" })
        vim.keymap.set("n", "<leader><leader>", function() telescope_builtin.find_files(dropdown(no_preview)) end,
            { desc = "Telescope Find files" })
        vim.keymap.set("n", ",,", function() telescope_builtin.current_buffer_fuzzy_find(no_preview) end,
            { desc = "Current File Search" })
        vim.keymap.set(
            "n",
            "<leader>o",
            function() telescope_builtin.treesitter(dropdown(no_preview)) end,
            { desc = "Search Symbols In Current File" }
        )
        vim.keymap.set("n", "??", function() telescope_builtin.live_grep(no_preview) end, { desc = "Live Grep" })
    end,
}
