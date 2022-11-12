require('telescope').setup{
    extensions = {
        file_browser = {
          -- disables netrw and use telescope-file-browser in its place
          hijack_netrw = true,
          mappings = {
            ["i"] = {
              -- your custom insert mode mappings
            },
            ["n"] = {
              -- your custom normal mode mappings
            },
          },
        },
    },
}

require"keymaps".bind {
    n = {
        ['<leader><leader>'] = "<cmd>Telescope find_files<CR>",
        ['<leader>fb'] = "<cmd>Telescope file_browser<CR>",
        ['??'] = '<cmd>Telescope live_grep<CR>',
        ['?a'] = '<cmd>lua vim.lsp.buf.code_action()<CR>',
    }
}
require("telescope").load_extension "file_browser"
