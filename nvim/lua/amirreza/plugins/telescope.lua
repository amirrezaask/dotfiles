return { -- Fuzzy finder
    {
        "nvim-telescope/telescope.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
            { "nvim-telescope/telescope-fzf-native.nvim", build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build" },
            "nvim-telescope/telescope-ui-select.nvim",
        },
        config = function()
            require("telescope").load_extension("ui-select") -- Use telescope for vim.ui.select
            local builtin = require("telescope.builtin")
            local no_preview = { previewer = false }
            local bind = function(mode, key, fn, desc)
                vim.keymap.set(mode, key, fn, { desc = "Telescope: " .. desc })
            end
            bind("n", "<C-p>", function() builtin.git_files(no_preview) end, "Git Files")

            bind("n", "<leader>i", function() builtin.find_files { cwd = vim.fn.stdpath("config")} end, "Edit Neovim Config")

            bind("n", "<leader>b", function() builtin.buffers(no_preview) end, "Buffers")

            bind("n", "<leader>/", function() builtin.current_buffer_fuzzy_find(no_preview) end,
                "Fuzzy find in current buffer")

            bind("n", "<leader><leader>", function() builtin.find_files(no_preview) end, "Fuzzy find")

            bind("n", "<leader>.",
                function() builtin.grep_string({ layout_config = { height = 0.7, width = 0.9 } }) end,
                "Grep current word")

            bind("n", "<leader>o", function() builtin.treesitter(no_preview) end, "Treesitter symbols")

            bind("n", "??",
                function() builtin.live_grep({ layout_config = { height = 0.9, width = 0.9 } }) end,
                "Grep in project")

            bind("n", "<leader>w", function() builtin.lsp_workspace_symbols(no_preview) end, "LSP workspace symbols")
        end,
    },

}
