local lspconfig = require('lspconfig')
local completion = require('completion')

require'lspinstall'.setup() -- important

local servers = require'lspinstall'.installed_servers()
for _, server in pairs(servers) do
  require'lspconfig'[server].setup{}
end

vim.cmd [[ autocmd BufEnter * lua require'completion'.on_attach() ]]

-- Commands
vim.cmd [[ command! LspDef lua vim.lsp.buf.definition() ]]
vim.cmd [[ command! LspHover lua vim.lsp.buf.hover() ]]
vim.cmd [[ command! LspImpl lua vim.lsp.buf.implementation() ]]
vim.cmd [[ command! LspSignatureHelp lua vim.lsp.buf.signature_help() ]]
vim.cmd [[ command! LspTypeDef lua vim.lsp.buf.type_definition() ]]
vim.cmd [[ command! LspRefs lua vim.lsp.buf.references() ]]
vim.cmd [[ command! LspDocSyms lua vim.lsp.buf.document_symbol() ]]
vim.cmd [[ command! LspWorkSyms lua vim.lsp.buf.workspace_symbol() ]]
vim.cmd [[ command! LspDecl lua vim.lsp.buf.declaration() ]]
vim.cmd [[ command! LspRename lua vim.lsp.buf.rename() ]]

-- Keybindings
vim.cmd [[ nnoremap <silent> gd    <cmd>LspDef<CR> ]]
vim.cmd [[ nnoremap <silent> K     <cmd>LspHover<CR> ]]
vim.cmd [[ nnoremap <silent> gI    <cmd>LspImpl<CR> ]]
vim.cmd [[ nnoremap <silent> <c-k> <cmd>LspSignatureHelp<CR> ]]
vim.cmd [[ nnoremap <silent> 1gD   <cmd>LspTypeDef<CR> ]]
vim.cmd [[ nnoremap <silent> gR    <cmd>LspRefs<CR> ]]
vim.cmd [[ nnoremap <silent> g0    <cmd>LspDocSyms<CR> ]]
vim.cmd [[ nnoremap <silent> gW    <cmd>LspWorkSyms<CR> ]]
vim.cmd [[ nnoremap <silent> gD    <cmd>LspDecl<CR> ]]
vim.cmd [[ nnoremap <silent> r     <cmd>LspRename<CR> ]]

-- Use <Tab> and <S-Tab> to navigate through popup menu
vim.cmd [[inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"]]
vim.cmd [[inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"]]

-- Set completeopt to have a better completion experience
vim.cmd [[set completeopt=menuone,noinsert,noselect]]

-- Avoid showing message extra message when using completion
vim.cmd [[set shortmess+=c]]


