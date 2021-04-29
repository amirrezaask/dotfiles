local completion = require('completion')

local function setup()
  vim.g.completion_enable_snippet = 'snippets.nvim'
  vim.cmd [[ inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>" ]]
  vim.cmd [[ inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>" ]]
  vim.cmd [[ set completeopt=menuone,noinsert,noselect ]]
  vim.cmd [[ set shortmess+=c ]]
end

return {
  on_attach = completion.on_attach,
  setup = setup
}
