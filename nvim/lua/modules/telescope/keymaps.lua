require"core.keymaps".bind {
    n = {
        ['<leader><leader>'] = "<cmd>Telescope find_files<CR>",
        ['??'] = '<cmd>Telescope live_grep<CR>',
        ['?a'] = '<cmd>lua vim.lsp.buf.code_action()<CR>',
    }
}
