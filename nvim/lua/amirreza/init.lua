require "amirreza.options"
require "amirreza.keymaps"
if vim.g.neovide then
    require "amirreza.neovide"
end

-- Highlight on Yank
vim.api.nvim_create_autocmd('TextYankPost', {
    group = vim.api.nvim_create_augroup('YankHighlight', { clear = true }),
    callback = function()
        vim.highlight.on_yank()
    end,
})

-- Transparency Control
TRANSPARENT = false

require "amirreza.plugin_manager"

if TRANSPARENT then
    vim.cmd [[
        hi! Normal guibg=none
        hi! NormalFloat guibg=none
    ]]
end
