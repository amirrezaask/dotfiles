local nnoremap = require("amirreza.keymap").nnoremap

-- Telescope
local telescope_builtin = require "telescope.builtin"
nnoremap('<leader><leader>', function() telescope_builtin.find_files() end)
nnoremap('??', '<cmd>Telescope live_grep<CR>')
nnoremap('?c', '<cmd>lua vim.lsp.buf.code_action()<CR>')


