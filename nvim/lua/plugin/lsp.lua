local lspconfig = require('lspconfig')

local function get_lua_runtime()
    local result = {};
    for _, path in pairs(vim.api.nvim_list_runtime_paths()) do
        local lua_path = path .. "/lua/";
        if vim.fn.isdirectory(lua_path) then
            result[lua_path] = true
        end
    end

    -- This loads the `lua` files from nvim into the runtime.
    result[vim.fn.expand("$VIMRUNTIME/lua")] = true

    -- TODO: Figure out how to get these to work...
    --  Maybe we need to ship these instead of putting them in `src`?...
    result[vim.fn.expand("~/build/neovim/src/nvim/lua")] = true
    result[vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true

    return result;
end

local telescope_on_attach = require('plugin.telescope').on_attach
local support_formatting = {'go', 'rust'}

local function make_on_attach(base)
  return function(_, _)
    if base then base() end
    vim.nmap {
      ['<leader>lR'] = vim.lsp.buf.rename,
      [',r'] = vim.lsp.buf.rename,
      ['K'] = vim.lsp.buf.hover,
      [',dn'] = vim.lsp.diagnostic.goto_next,
      [',dp'] = vim.lsp.diagnostic.goto_prev,
      [',dl'] = vim.lsp.diagnostic.show_line_diagnostics,
    }
    local filetype = vim.api.nvim_buf_get_option(0, 'filetype')
    if vim.tbl_contains(support_formatting, filetype) then
      vim.autocmd {
        'BufWritePre', '<buffer>', vim.lsp.buf.formatting_sync
      }
    end
  end
end
local on_attach = make_on_attach(telescope_on_attach)

lspconfig.gopls.setup({ 
  on_attach = on_attach
})
lspconfig.rust_analyzer.setup({
  on_attach = function()
    on_attach()
    require('lsp_extensions').inlay_hints({})
  end,
})

local sumneko_root = '/home/amirreza/.local/lua-language-server'
local sumneko_binary = sumneko_root .. '/bin/Linux/lua-language-server'

lspconfig.sumneko_lua.setup({
    cmd = { sumneko_binary, '-E', sumneko_root .. '/main.lua' },
    -- Lua LSP configuration
    settings = {
      Lua = {
        runtime = {
          version = "LuaJIT",
        },

        completion = {
          keywordSnippet = "Disable",
        },

        diagnostics = {
          enable = true,
          disable = {"trailing-space"},
          globals = {
              "vim",
              "describe", "it", "before_each", "after_each", "teardown", "pending", "clear",
              'awesome', 'client'
            }
        },

        workspace = {
          library = get_lua_runtime(),
          maxPreload = 1000,
          preloadFileSize = 1000,
        },
      }
    },

    -- Runtime configurations
    filetypes = {"lua"},
    on_attach = on_attach,
  })

lspconfig.pyls_ms.setup({ on_attach = on_attach })
lspconfig.clangd.setup({ on_attach = on_attach })

