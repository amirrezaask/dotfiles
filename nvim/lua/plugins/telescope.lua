require("telescope").setup {
    defaults = {
        layout_strategy = "flex",
        sorting_strategy = "ascending",
        prompt_position = "top",
        layout_config = {
            prompt_position = "top",
        }
    }
}

require('telescope').load_extension('fzf')
require("telescope").load_extension("ui-select")

local builtin = require("telescope.builtin")


if g.fuzzy_finder == 'telescope' then
    vim.lsp.buf.definition = builtin.lsp_definitions
    vim.lsp.buf.references = builtin.lsp_references
    vim.lsp.buf.implementation = builtin.lsp_implementations

    vim.keymap.set("n", "<leader><leader>", builtin.find_files, { desc = "Find Files" })
    vim.keymap.set("n", "<leader>b", builtin.buffers, { desc = "Find Buffers" })
    vim.keymap.set("n", "<leader>h", builtin.help_tags, { desc = "Vim Help Tags" })
    vim.keymap.set("n", "<C-p>", builtin.git_files, { desc = "Git Files" })
    vim.keymap.set("n", "??", builtin.live_grep, { desc = "Live Grep" })
    vim.keymap.set("n", "<leader>fw", function()
        vim.ui.input({ prompt = "Grep>" }, function(word)
            if word == nil or word == "" then
                return
            end
            builtin.grep_string { search = word }
        end)
    end
    , { desc = "Grep" })
    vim.keymap.set("v", "??", builtin.grep_string, { desc = "Grep word under cursor" })
    vim.keymap.set("n", "<leader>o", builtin.lsp_document_symbols, { desc = "LSP Document Symbols" })
    vim.keymap.set("n", "<leader>O", builtin.lsp_workspace_symbols, { desc = "LSP Workspace Symbols" })
    vim.keymap.set("n", "<M-o>", builtin.lsp_workspace_symbols, { desc = "LSP Workspace Symbols" })
    vim.keymap.set("n", "<leader>fd", function() builtin.find_files({ cwd = "~/.dotfiles" }) end,
        { desc = "Find Dotfiles" })
end
