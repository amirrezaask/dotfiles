if ! exists("g:loaded_telescope")
    finish
endif

nnoremap <leader><leader> :lua require"amirrezaask.telescope".wrap(require("telescope.builtin").find_files, { hidden = true })()<CR>
nnoremap <leader>fb :lua require"amirrezaask.telescope".wrap(require("telescope.builtin").file_browser)()<CR>
nnoremap <leader>fp :lua require"amirrezaask.telescope".installed_plugins()<CR>
nnoremap <leader>pp :lua require"amirrezaask.telescope".projects()<CR>
nnoremap <leader>ps :lua require"amirrezaask.telescope".snapp()<CR>
nnoremap <C-p> :lua require"amirrezaask.telescope".git_files()<CR>
nnoremap <C-q> :lua require"amirrezaask.telescope".quickfix()<CR>
nnoremap ?? :lua require"amirrezaask.telescope".wrap(require("telescope.builtin").live_grep)()<CR>
nnoremap ,c :lua require"amirrezaask.telescope".edit_configs()<CR>
nnoremap <leader>ec :lua require"amirrezaask.telescope".edit_configs()<CR>
nnoremap <leader>L  :lua require"amirrezaask.telescope".telescope_commands()<CR>
nnoremap <leader>en :lua require"amirrezaask.telescope".edit_neovim()<CR>
nnoremap ,n :lua require"amirrezaask.telescope".edit_neovim()<CR>
nnoremap gd :Telescope lsp_definitions<CR>
nnoremap gr :Telescope lsp_references<CR>
nnoremap gi :Telescope lsp_implementations<CR>
nnoremap ?d :Telescope lsp_document_symbols<CR>
nnoremap ?w :Telescope lsp_workspace_symbols<CR>
nnoremap ?c :Telescope lsp_code_actions<CR>
