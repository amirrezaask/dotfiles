augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
augroup end

autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

let g:netrw_browse_split = 0
let g:netrw_banner = 0
let g:netrw_winsize = 25


