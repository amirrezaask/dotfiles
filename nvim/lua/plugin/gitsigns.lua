require('gitsigns').setup({
  signs = {
    add = {  text = '+', numhl = 'GitSignsAddNr' },
    change = {  text = '~', numhl = 'GitSignsChangeNr' },
    delete = {  text = '_', numhl = 'GitSignsDeleteNr' },
    topdelete = {  text = '‾', numhl = 'GitSignsDeleteNr' },
    changedelete = {  text = '~-', numhl = 'GitSignsChangeNr' },
  },
})


-- nnoremap <M-j> :m .+1<CR>==
-- nnoremap <M-k> :m .-2<CR>==
-- inoremap <M-j> <Esc>:m .+1<CR>==gi
-- inoremap <M-k> <Esc>:m .-2<CR>==gi
-- vnoremap <M-j> :m '>+1<CR>gv=gv
-- vnoremap <M-k> :m '<-2<CR>gv=gv
