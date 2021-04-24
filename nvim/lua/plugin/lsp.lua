local lspconfig = require('lspconfig')

local function get_lua_runtime()
  local result = {}
  for _, path in pairs(vim.api.nvim_list_runtime_paths()) do
    local lua_path = path .. '/lua/'
    if vim.fn.isdirectory(lua_path) then
      result[lua_path] = true
    end
  end

  result[vim.fn.expand('$VIMRUNTIME/lua')] = true

  result[vim.fn.expand('~/build/neovim/src/nvim/lua')] = true

  return result
end

lspconfig.gopls.setup({})
lspconfig.rust_analyzer.setup({
  on_attach = function()
    require('lsp_extensions').inlay_hints({})
  end,
})

local sumneko_root = '/home/amirreza/.local/lua-language-server'
local sumneko_binary = sumneko_root .. '/bin/Linux/lua-language-server'
require('nlua.lsp.nvim').setup(require('lspconfig'), {
  cmd = { sumneko_binary, '-E', sumneko_root .. '/main.lua' },
  globals = {
    'vim',
  },
})

lspconfig.pyls_ms.setup({})
lspconfig.clangd.setup({})
