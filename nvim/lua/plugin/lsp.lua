local lspconfig = require('lspconfig')

require('lspsaga').init_lsp_saga {
  use_saga_diagnostic_sign = false,
  code_action_prompt = {
    enable = false,
    sign = true,
    virtual_text = true,
  },
  finder_action_keys = {
    open = 'o', vsplit = 's',split = 'i',quit = 'q',scroll_down = '<C-f>', scroll_up = '<C-b>'
  },
  code_action_keys = {
    quit = 'q',exec = '<CR>'
  },
  rename_action_keys = {
    quit = '<C-c>',exec = '<CR>'
  },
  server_filetype_map = {}
}

local on_attach

if package.loaded['plugin.fzf'] then
  on_attach = require('plugin.fzf').lsp_on_attach
elseif package.loaded['plugin.telescope'] then
  on_attach = require('plugin.telescope').on_attach
end

local function global_on_attach(inner)
  return function()
    inner()
    vim.cmd[[ nnoremap <silent><leader>lc <cmd>lua require('lspsaga.codeaction').code_action()<CR> ]]
    vim.cmd [[ vnoremap <silent><leader>lc :<C-U>lua require('lspsaga.codeaction').range_code_action()<CR> ]]
    vim.cmd [[ nnoremap <silent><leader>lR <cmd>lua require('lspsaga.rename').rename()<CR> ]]
    vim.cmd [[ nnoremap <silent><leader>d? <cmd>lua require'lspsaga.diagnostic'.show_line_diagnostics()<CR> ]]
    vim.cmd [[ nnoremap <silent> [e <cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_prev()<CR> ]]
    vim.cmd [[ nnoremap <silent> ]e <cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_next()<CR> ]]
  end
end

on_attach = global_on_attach(on_attach)

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
