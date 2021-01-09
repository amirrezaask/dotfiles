local nvim_lsp = require('lspconfig')
local completion = require('completion')

-- nvim_lsp.pyls.setup{}
nvim_lsp.gopls.setup{}

nvim_lsp.elixirls.setup{
  cmd = { "/home/amirreza/bin/elixirls/language_server.sh" } 
}
vim.cmd [[ autocmd BufEnter * lua require'completion'.on_attach() ]]

-- Keybindings 
vim.cmd [[ nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR> ]]
vim.cmd [[ nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR> ]]
vim.cmd [[ nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR> ]]
vim.cmd [[ nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR> ]]
vim.cmd [[ nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR> ]]
vim.cmd [[ nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR> ]] 
vim.cmd [[ nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR> ]]
vim.cmd [[ nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR> ]]
vim.cmd [[ nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR> ]]

-- Use <Tab> and <S-Tab> to navigate through popup menu
vim.cmd [[inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"]]
vim.cmd [[inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"]]

-- Set completeopt to have a better completion experience
vim.cmd [[set completeopt=menuone,noinsert,noselect]]

-- Avoid showing message extra message when using completion
vim.cmd [[set shortmess+=c]]


