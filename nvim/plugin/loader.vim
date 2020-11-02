lua require'lsp'
lua require'runner'
lua require'statusline'
lua require'tsitter'
lua require'music_player'.new(require'music_player.rhythmbox')
lua require'_snippets'
nnoremap <leader>s <cmd>lua require'sidetree'.open_side_file_browser()<CR>
lua require('colorbuddy').colorscheme('gruvbuddy')
" colorscheme bluescreen
