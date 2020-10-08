lua require'colorbuddy'.colorscheme('gruvbuddy')
lua require'el'.setup{}
lua require'lsp'
lua require'fuzzy.cmds'(vim.api.nvim_get_var('fuzzy_backend'))
