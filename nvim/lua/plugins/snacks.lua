require("snacks").setup {
    bigfile = { enabled = true },
    picker = { enabled = true }
}

Picker = require("snacks").picker

if vim.fn.executable("fzf") == 0 then
    LspDeclaration = Picker.lsp_declarations
    LspDefinition = Picker.lsp_definitions
    LspReferences = Picker.lsp_references
    LspImplementation = Picker.lsp_implementations

    vim.keymap.set("n", "<leader><leader>", Picker.files, { desc = "Find Files" })
    vim.keymap.set("n", "<leader>b", Picker.buffers, { desc = "Find Buffers" })
    vim.keymap.set("n", "<leader>h", Picker.help, { desc = "Vim Help Tags" })
    vim.keymap.set("n", "<C-p>", Picker.git_files, { desc = "Git Files" })
    vim.keymap.set("n", "??", Picker.grep, { desc = "Live Grep" })
    vim.keymap.set("n", "<leader>fw", Picker.grep, { desc = "Grep" })
    vim.keymap.set("v", "??", Picker.grep_word, { desc = "Grep word under cursor" })
    vim.keymap.set("n", "<leader>o", Picker.lsp_symbols, { desc = "LSP Document Symbols" })
    vim.keymap.set("n", "<leader>O", Picker.lsp_workspace_symbols, { desc = "LSP Workspace Symbols" })
    vim.keymap.set("n", "<M-o>", Picker.lsp_workspace_symbols, { desc = "LSP Workspace Symbols" })
    vim.keymap.set("n", "<leader>fd", function() Picker.files({ cwd = "~/.dotfiles" }) end, { desc = "Find Dotfiles" })
end
