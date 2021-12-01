if ! exists("g:loaded_telescope")
    finish
endif

nnoremap <leader><leader> <cmd>lua require"amirrezaask.telescope".wrap("find_files", { hidden = true })<CR>
nnoremap <leader>fb <cmd>lua require"amirrezaask.telescope".wrap("file_browser")<CR>
nnoremap <leader>fp <cmd>lua require"amirrezaask.telescope".installed_plugins()<CR>
nnoremap <leader>pp <cmd>lua require"amirrezaask.telescope".projects()<CR>
nnoremap <leader>ps <cmd>lua require"amirrezaask.telescope".snapp()<CR>
nnoremap <C-p> <cmd>lua require"amirrezaask.telescope".git_files()<CR>
nnoremap <C-q> <cmd>lua require"amirrezaask.telescope".quickfix()<CR>
nnoremap ?? <cmd>lua require"amirrezaask.telescope".wrap("live_grep")<CR>
nnoremap ,c <cmd>lua require"amirrezaask.telescope".edit_configs()<CR>
nnoremap <leader>ec <cmd>lua require"amirrezaask.telescope".edit_configs()<CR>
nnoremap <leader>L  <cmd>lua require"amirrezaask.telescope".telescope_commands()<CR>
nnoremap <leader>en <cmd>lua require"amirrezaask.telescope".edit_neovim()<CR>
nnoremap ,n <cmd>lua require"amirrezaask.telescope".edit_neovim()<CR>
" LSP
nnoremap gd <cmd>lua require"amirrezaask.telescope".wrap('lsp_definitions')<CR>
nnoremap gr <cmd>lua require"amirrezaask.telescope".wrap('lsp_references')<CR>
nnoremap gi <cmd>lua require"amirrezaask.telescope".wrap('lsp_implementations')<CR>
nnoremap ?d <cmd>lua require"amirrezaask.telescope".wrap('lsp_document_symbols')<CR>
nnoremap ?w <cmd>lua require"amirrezaask.telescope".wrap('lsp_workspace_symbols')<CR>
nnoremap ?c <cmd>lua require"amirrezaask.telescope".wrap('lsp_code_actions')<CR>
