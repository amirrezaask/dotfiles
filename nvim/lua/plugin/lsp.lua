local lspconfig = require('lspconfig')
local on_attach

if package.loaded['plugin.fzf'] then
  on_attach = require('plugin.fzf').lsp_on_attach
elseif package.loaded['plugin.telescope'] then
  on_attach = require('plugin.telescope').on_attach
end

lspconfig.gopls.setup({ on_attach = on_attach })
lspconfig.rust_analyzer.setup({
  on_attach = function()
    on_attach()
    require('lsp_extensions').inlay_hints({})
  end,
})

local sumneko_root = '/home/amirreza/.local/lua-language-server'
local sumneko_binary = sumneko_root .. '/bin/Linux/lua-language-server'
require('nlua.lsp.nvim').setup(require('lspconfig'), {
  on_attach = on_attach,
  cmd = { sumneko_binary, '-E', sumneko_root .. '/main.lua' },
  globals = {
    'vim',
    'awesome',
  },
})

lspconfig.pyls_ms.setup({ on_attach = on_attach })
lspconfig.clangd.setup({ on_attach = on_attach })
