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


nvim.mode_map {
  n = {
    ['gd'] = '<cmd>LSPDefinitions<CR>',
    ['K'] = '<cmd>LSPHover<CR>',
    ['gI'] = '<cmd>LSPImplementations<CR>',
    ['<c-k>'] = '<cmd>LSPSignatureHelp<CR>',
    ['1gD'] = '<cmd>LSPTypeDefinition<CR>',
    ['gR'] = '<cmd>LSPReferences<CR>',
    ['g0'] = '<cmd>LSPDocumentSymbols<CR>',
    ['gW'] = '<cmd>LSPWorkspaceSymbols<CR>',
    ['gD'] = '<cmd>LSPDeclaration<CR>',
    ['<Space>A'] = '<cmd>LSPCodeActions<CR>',
    ['<Space>R'] = '<cmd>LSPRename<CR>',
  },
  i = {
    ['<expr> <C-Space>'] = 'compe#complete()',
    ['<expr> <CR>'] = "compe#confirm('<CR>')",
    ['<expr> <C-e>'] = "compe#close('<C-e>')",
    ['<expr> <C-f>'] = "compe#scroll( {'delta': +4} )",
    ['<expr> <C-d>'] = "compe#scroll( {'delta': -4} )",
  }
}
vim.cmd [[inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"]]
vim.cmd [[inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"]]

-- Set completeopt to have a better completion experience
vim.cmd [[set completeopt=menuone,noselect]]

-- Avoid showing message extra message when using completion
vim.cmd [[set shortmess+=c]]



