Fzf = require("fzf-lua")
Fzf.setup {
    fzf_colors = true,
    keymap = {
        fzf = {
            ["ctrl-q"] = "select-all+accept", -- Select all items and send to quickfix
        },
    },
}
Fzf.register_ui_select()

if g.fuzzy_finder == 'fzf' then
    vim.keymap.set("n", "<leader><leader>", Fzf.files, { desc = "Find Files" })
    vim.keymap.set("n", "<leader>b", Fzf.buffers, { desc = "Find Buffers" })
    vim.keymap.set("n", "<leader>h", Fzf.helptags, { desc = "Vim Help Tags" })
    vim.keymap.set("n", "<C-p>", Fzf.git_files, { desc = "Git Files" })
    vim.keymap.set("n", "??", Fzf.live_grep, { desc = "Live Grep" })
    vim.keymap.set("n", "<leader>fw", Fzf.grep, { desc = "Grep" })
    vim.keymap.set("v", "??", Fzf.grep_cword, { desc = "Grep word under cursor" })
    vim.keymap.set("n", "<leader>o", Fzf.lsp_document_symbols, { desc = "LSP Document Symbols" })
    vim.keymap.set("n", "<leader>O", Fzf.lsp_live_workspace_symbols, { desc = "LSP Workspace Symbols" })
    vim.keymap.set("n", "<M-o>", Fzf.lsp_live_workspace_symbols, { desc = "LSP Workspace Symbols" })
    vim.keymap.set("n", "<leader>fd", function() Fzf.files({ cwd = "~/.dotfiles" }) end, { desc = "Find Dotfiles" })


    LspDeclaration = Fzf.lsp_declarations
    LspDefinition = Fzf.lsp_definitions
    LspReferences = Fzf.lsp_references
    LspImplementation = Fzf.lsp_implementations
end
