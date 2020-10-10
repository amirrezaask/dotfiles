lua require'el'.setup{}
lua require'lsp'
lua require'fuzzy.cmds'(vim.api.nvim_get_var('fuzzy_backend'))
lua require'start'.set_background_ascii(require'start'.location.center, require'start'.neovim_is_awesome)
