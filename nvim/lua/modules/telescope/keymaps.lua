require"core.keymaps".bind {
    n = {
        ['<leader><leader>'] = "<cmd>Telescope find_files<CR>",
        ['<leader>fb'] = "<cmd>Telescope file_browser<CR>",
        ['??'] = '<cmd>Telescope live_grep<CR>',
        ['?a'] = '<cmd>lua vim.lsp.buf.code_action()<CR>',
    }
}
