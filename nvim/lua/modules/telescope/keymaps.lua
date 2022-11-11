local bind = require"core.keymaps".bind
local telescope_builtin = require "telescope.builtin"

bind {
    n = {
        ['<leader><leader>'] = function() telescope_builtin.find_files() end,
        ['<leader>fd'] = function() telescope_builtin.file_browser() end,
        ['??'] = '<cmd>Telescope live_grep<CR>',
        ['?a'] = '<cmd>lua vim.lsp.buf.code_action()<CR>',
    }
}
