lua require'lsp'
lua require'runner'
lua require'fuzzy.cmds'(vim.api.nvim_get_var('fuzzy_backend'))
lua require'start'.set_background_ascii(require'start'.location.center, require'start'.neovim_is_awesome)
lua require'colorizer'.setup{}
