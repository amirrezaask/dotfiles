local nvim_lsp = require('nvim_lsp')
local completion = require('completion')

nvim_lsp.gopls.setup{}
nvim_lsp.pyls.setup{}
nvim_lsp.intelephense.setup{}


local function completion_on_all_buffers()
    completion.on_attach()    
end


return {
    all_buffers = completion_on_all_buffers
}
