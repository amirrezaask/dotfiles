local has_lspconfig, lspconfig = pcall(require,'lspconfig')
if not has_lspconfig then return end
local lspconfig_util = require('lspconfig.util')

local has_lspkind, lspkind = pcall(require, 'lspkind')
if has_lspkind then
  lspkind.init()
end

local M = {}

local has_lspstatus, lspstatus = pcall(require, 'lsp-status')
if has_lspstatus then
  lspstatus.config({
    indicator_errors = '😡',
    indicator_warnings = '😳',
    indicator_info = '🛈',
    indicator_hint = '😅',
    indicator_ok = '🆗',
    status_symbol = '',
  })
end

local function get_root(...)
  return lspconfig_util.root_pattern(...)(vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()))
end

function M.go_root()
  local go_root_patterns = {'go.mod', '.git'}
  get_root(unpack(go_root_patterns))
end

local function get_lua_runtime()
    local result = {};
    for _, path in pairs(vim.api.nvim_list_runtime_paths()) do
        local lua_path = path .. "/lua/";
        if vim.fn.isdirectory(lua_path) then
            result[lua_path] = true
        end
    end

    result[vim.fn.expand("$VIMRUNTIME/lua")] = true
    result[vim.fn.expand('$VIMRUNTIME/lua/vim')] = true
    return result;
end

local function rename()
  local current_word = vim.fn.expand('<cword>')
  require('floating'):prompt(string.format('Rename %s to > ', current_word), vim.lsp.buf.rename)
end

local telescope_on_attach = require('plugin.telescope').on_attach
local support_formatting = {'go', 'rust'}

local function make_on_attach(base)
  return function(client)
    if base then base() end
    if has_lspstatus and client then
      lspstatus.on_attach(client)
    end
    vim.nmap {
      ['<leader>lR'] = { rename, "Rename current symbol under cursor", "IDE" },
      [',r'] = { rename, "Rename current symbol under cursor", "IDE" },
      ['K'] = { vim.lsp.buf.hover, "Hover information about symbol under cursor", "IDE" },
      [',dn'] = { vim.lsp.diagnostic.goto_next, "Goto next diagnostic", "IDE" },
      [',dp'] = { vim.lsp.diagnostic.goto_prev, "Goto previous diagnostic", "IDE"},
      [',dl'] = { vim.lsp.diagnostic.show_line_diagnostics, "Show current line diagnostic", "IDE" },
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
  on_attach = function(client)
    on_attach(client)
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
    on_attach = function(client)
      on_attach(client)
      vim.lsp.handlers['textDocument/hover'] = function(_, _, _)

        local original_iskeyword = vim.bo.iskeyword
        vim.bo.iskeyword = vim.bo.iskeyword .. ',.'
        local word = vim.fn.expand("<cword>")
        vim.bo.iskeyword = original_iskeyword

        if string.find(word, 'vim.api') then
          local _, finish = string.find(word, 'vim.api.')
          local api_function = string.sub(word, finish + 1)

          vim.cmd(string.format('help %s', api_function))
          return
        elseif string.find(word, 'vim.fn') then
          local _, finish = string.find(word, 'vim.fn.')
          local api_function = string.sub(word, finish + 1) .. '()'

          vim.cmd(string.format('help %s', api_function))
          return
        else
          local ok = pcall(vim.cmd, string.format('help %s', word))

          if not ok then
            local split_word = vim.split(word, '.', true)
            ok = pcall(vim.cmd, string.format('help %s', split_word[#split_word]))
          end
          if not ok then
            vim.lsp.buf.hover()
          end
        end
      end
    end,
  })

lspconfig.pyls_ms.setup({ on_attach = on_attach })
lspconfig.clangd.setup({ on_attach = on_attach })
lspconfig.intelephense.setup({ on_attach = on_attach })

return M
