local nvim_lsp = require('nvim_lsp')
local completion = require('completion')

nvim_lsp.gopls.setup{}
nvim_lsp.pyls.setup{}
nvim_lsp.intelephense.setup{}
-- Lua setup
require('nlua.lsp.nvim').setup(require('nvim_lsp'), {
  on_attach = custom_nvim_lspconfig_attach,

  -- Include globals you want to tell the LSP are real :)
  globals = {
    -- Colorbuddy
    "Color", "c", "Group", "g", "s",
  }
})

local function completion_on_all_buffers()
    completion.on_attach()
end


return {
    all_buffers = completion_on_all_buffers
}
