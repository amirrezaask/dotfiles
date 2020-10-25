lua require'lsp'
lua require'runner'
lua require'statusline'
lua require'start'.set_background_ascii(require'start'.location.center, require'start'.neovim_is_awesome)
lua require'colorizer'.setup{}
lua require'colorbuddy'.colorscheme('gruvbuddy')
lua require'music_player'.new(require'music_player.rhythmbox')
lua require'nvim-web-devicons'.setup()
lua require'_snippets'
