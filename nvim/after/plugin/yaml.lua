require"treesitter".install("yaml")
vim.cmd [[
    autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
    autocmd FileType yaml setlocal cursorcolumn
]]
