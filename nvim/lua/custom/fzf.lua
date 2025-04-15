return function()
    Fzf = require("fzf-lua")
    Fzf.setup { fzf_colors = true }
    Fzf.register_ui_select()
    vim.keymap.set("n", "<leader><leader>", Fzf.files)
    vim.keymap.set("n", "<leader>b", Fzf.buffers)
    vim.keymap.set("n", "<leader>h", Fzf.helptags)
    vim.keymap.set("n", "<C-p>", Fzf.git_files)
    vim.keymap.set("n", "??", Fzf.live_grep)
    vim.keymap.set("v", "??", Fzf.grep_cword)
    vim.keymap.set("n", "<leader>o", Fzf.lsp_document_symbols)
    vim.keymap.set("n", "<leader>O", Fzf.lsp_live_workspace_symbols)
    vim.keymap.set("n", "<leader>fd", function() Fzf.files({ cwd = "~/.dotfiles" }) end)
end
