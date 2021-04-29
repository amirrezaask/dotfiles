local lspconfig = require('lspconfig')

local function on_attach(_)
  require('amirrezaask.nvim').mode_map({
    n = {
      ['gd'] = function()
        require('telescope.builtin').lsp_definitions(require('plugin.telescope').vertical_opts)
      end,
      ['K'] = vim.lsp.buf.hover,
      ['gI'] = function()
        require('telescope.builtin').lsp_implementations(require('plugin.telescope').vertical_opts)
      end,
      ['gR'] = function()
        require('telescope.builtin').lsp_references(require('plugin.telescope').vertical_opts)
      end,
      ['<leader>lr'] = function()
        require('telescope.builtin').lsp_references(require('plugin.telescope').vertical_opts)
      end,
      ['<leader>li'] = function()
        require('telescope.builtin').lsp_implementations(require('plugin.telescope').vertical_opts)
      end,
      ['<leader>ld'] = function()
        require('telescope.builtin').lsp_document_symbols(require('plugin.telescope').vertical_opts)
      end,
      ['<leader>lw'] = function()
        require('plugin.telescope').lsp_workspace_symbols()
      end,
      ['<leader>lc'] = function()
        require('telescope.builtin').lsp_code_actions()
      end,
      ['<leader>d?'] = function()
        require('telescope.builtin').lsp_document_diagnostics()
      end,
      ['<leader>w?'] = function()
        require('telescope.builtin').lsp_workspace_diagnostics()
      end,
    },
  })
  require'plugin.completion'.on_attach()
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
