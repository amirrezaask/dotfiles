local lspconfig = require('lspconfig')

local nvim = require'nvim'

local function get_lua_runtime()
    local result = {};
    for _, path in pairs(vim.api.nvim_list_runtime_paths()) do
        local lua_path = path .. "/lua/";
        if vim.fn.isdirectory(lua_path) then
            result[lua_path] = true
        end
    end

    result[vim.fn.expand("$VIMRUNTIME/lua")] = true

    result[vim.fn.expand("~/build/neovim/src/nvim/lua")] = true

    return result;
end

lspconfig.gopls.setup{}
lspconfig.rust_analyzer.setup{
  on_attach = function()
    require'lsp_extensions'.inlay_hints{}
  end
}

local sumneko_root = "/home/amirreza/.local/lua-language-server"
local sumneko_binary = sumneko_root .. "/bin/Linux/lua-language-server" 

lspconfig.sumneko_lua.setup{
  cmd = {sumneko_binary, "-E", sumneko_root .. "/main.lua"};
  settings = {
    Lua = {
      runtime = {
        version = "LuaJIT"
      },
      diagnostics = {
        enable = true,
        disable = {
          "trailing-space",
        },
        globals = {"vim"}
      },
      workspace = {
        library = vim.list_extend(get_lua_runtime(), {}),
        maxPreload = 1000,
        preloadFileSize = 1000,
      },
    }
  }
}

lspconfig.pyls_ms.setup{}

require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = true;

  source = {
    path = true;
    buffer = true;
    calc = true;
    nvim_lsp = true;
    nvim_lua = true;
    vsnip = true;
  };
}
-- Commands

-- if package.loaded['fzf'] then
--   nvim.command('LSPDefinitions', LSPDefinitions)
--   nvim.command('LSPHover', vim.lsp.buf.hover)
--   nvim.command('LSPSignatureHelp', vim.lsp.buf.signature_help)
--   nvim.command('LSPTypeDefinition', vim.lsp.buf.type_definition)
--   nvim.command('LSPRename', require'lspsaga.rename'.rename)
--   nvim.command('LSPWorkspaceSymbols', LSPWorkspace_Symbols)
--   nvim.command('LSPDocumentSymbols', LSP_Document_Symbols)
--   nvim.command('LSPReferences', LSPReferences)
--   nvim.command('LSPImplementations', LSPImplementations)
--   nvim.command('LSPCodeActions', LSPCodeActions)
--   nvim.command('LSPDeclaration', LSPDeclaration)
-- end

local fuzzy_lsp = require'fuzzy.lsp'
nvim.command('LSPDefinitions', fuzzy_lsp.definitions)
nvim.command('LSPHover', vim.lsp.buf.hover)
nvim.command('LSPSignatureHelp', vim.lsp.buf.signature_help)
nvim.command('LSPTypeDefinition', vim.lsp.buf.type_definition)
nvim.command('LSPRename', require'lspsaga.rename'.rename)
nvim.command('LSPWorkspaceSymbols', fuzzy_lsp.workspace_symbols)
nvim.command('LSPDocumentSymbols', fuzzy_lsp.document_symbols)
nvim.command('LSPReferences', fuzzy_lsp.references)
nvim.command('LSPImplementations', fuzzy_lsp.implementation)
nvim.command('LSPCodeActions', fuzzy_lsp.code_actions)
nvim.command('LSPDeclaration', fuzzy_lsp.declaration)

-- Keybindings
vim.cmd [[ nnoremap <silent> gd    <cmd>LSPDefinitions<CR> ]]
vim.cmd [[ nnoremap <silent> K     <cmd>LSPHover<CR> ]]
vim.cmd [[ nnoremap <silent> gI    <cmd>LSPImplementations<CR> ]]
vim.cmd [[ nnoremap <silent> <c-k> <cmd>LSpSignatureHelp<CR> ]]
vim.cmd [[ nnoremap <silent> 1gD   <cmd>LSPTypeDefinition<CR> ]]
vim.cmd [[ nnoremap <silent> gR    <cmd>LSPReferences<CR> ]]
vim.cmd [[ nnoremap <silent> g0    <cmd>LSPDocumentSymbols<CR> ]]
vim.cmd [[ nnoremap <silent> gW    <cmd>LSPWorkspaceSymbols<CR> ]]
vim.cmd [[ nnoremap <silent> gD    <cmd>LSPDeclaration<CR> ]]
vim.cmd [[ nnoremap <silent> <Space>A    <cmd>LSPCodeActions<CR> ]]
vim.cmd [[ nnoremap <silent> <Space>R     <cmd>LSpRename<CR> ]]

vim.cmd [[ inoremap <silent><expr> <C-Space> compe#complete() ]]
vim.cmd [[ inoremap <silent><expr> <CR>      compe#confirm('<CR>') ]]
vim.cmd [[ inoremap <silent><expr> <C-e>     compe#close('<C-e>') ]]
vim.cmd [[ inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 }) ]]
vim.cmd [[ inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 }) ]]


-- Use <Tab> and <S-Tab> to navigate through popup menu
vim.cmd [[inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"]]
vim.cmd [[inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"]]

-- Set completeopt to have a better completion experience
vim.cmd [[set completeopt=menuone,noselect]]

-- Avoid showing message extra message when using completion
vim.cmd [[set shortmess+=c]]



