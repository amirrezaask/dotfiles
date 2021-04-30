require('compe').setup({
  enabled = true,
  autocomplete = true,
  debug = false,
  min_length = 1,
  preselect = 'enable',
  throttle_time = 80,
  source_timeout = 200,
  incomplete_delay = 400,
  max_abbr_width = 100,
  max_kind_width = 100,
  max_menu_width = 100,
  documentation = true,

  source = {
    path = true,
    buffer = true,
    calc = true,
    nvim_lsp = true,
    nvim_lua = true,
    snippets_nvim = true,
  },
})
vim.cmd([[inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"]])
vim.cmd([[inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"]])

-- Set completeopt to have a better completion experience
vim.cmd([[set completeopt=menuone,noselect]])

-- Avoid showing message extra message when using completion
vim.cmd([[set shortmess+=c]])

require('amirrezaask.nvim').mode_map({
  i = {
    ['<expr> <C-Space>'] = 'compe#complete()',
    ['<expr> <CR>'] = "compe#confirm('<CR>')",
    ['<expr> <C-e>'] = "compe#close('<C-e>')",
    ['<expr> <C-f>'] = "compe#scroll( {'delta': +4} )",
    ['<expr> <C-d>'] = "compe#scroll( {'delta': -4} )",
  },
})
