local has_compe, compe = pcall(require, 'compe')
if not has_compe then return end

compe.setup({
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
    calc = false,
    nvim_lsp = true,
    nvim_lua = true,
    snippets_nvim = true,
  },
})

-- Set completeopt to have a better completion experience
vim.opt.completeopt = {'menuone', 'noselect'}

-- Avoid showing message extra message when using completion
vim.opt.shortmess:append('c')

vim.imap {
  ['<expr> <S-Tab>'] = [[ pumvisible() ? "\<C-p>" : "\<S-Tab>"]],
  ['<expr> <Tab>'] = [[ pumvisible() ? "\<C-n>" : "\<Tab>" ]],
  ['<expr> <C-Space>'] = 'compe#complete()',
  ['<expr> <CR>'] = "compe#confirm()",
  ['<expr> <C-e>'] = "compe#close('<C-e>')",
  ['<expr> <C-f>'] = "compe#scroll( {'delta': +4} )",
  ['<expr> <C-d>'] = "compe#scroll( {'delta': -4} )",
}
