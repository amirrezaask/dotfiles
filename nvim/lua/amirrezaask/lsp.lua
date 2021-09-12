local has_lspconfig, lspconfig = pcall(require, "lspconfig")
if not has_lspconfig then
  return
end
local lspconfig_util = require "lspconfig.util"

local has_nlua, nlua = pcall(require, "nlua.lsp.nvim")

if not has_nlua then
  print "for better lua support install nlua"
  return
end
local has_lspkind, lspkind = pcall(require, "lspkind")

if has_lspkind then
  lspkind.init()
end

local M = {}

local function get_root(...)
  return lspconfig_util.root_pattern(...)(vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()))
end

function M.go_root()
  local go_root_patterns = { "go.mod", ".git" }
  get_root(unpack(go_root_patterns))
end

local function rename()
  local current_word = vim.fn.expand "<cword>"
  vim.lsp.buf.rename(vim.fn.input(string.format("Rename %s to > ", current_word)))
end

local support_formatting = { "rust" }

local function make_on_attach(base)
  return function(client)
    if base then
      base()
    end
    vim.imap {
      ["<c-s>"] = { vim.lsp.buf.signature_help, "" },
      ["<c-d>"] = { vim.lsp.diagnostic.show_line_diagnostics, "" },
    }
    vim.nmap {
      ["gd"] = { vim.lsp.buf.definition, "" },
      ["gi"] = { vim.lsp.buf.implementation, "" },
      ["gr"] = { vim.lsp.buf.references, "" },
      ["?d"] = { vim.lsp.buf.document_symbol, "" },
      ["?w"] = { vim.lsp.buf.workspace_symbol, "" },
      ["?c"] = { vim.lsp.buf.code_action, "" },

      ["<c-s>"] = { vim.lsp.buf.signature_help, "" },
      ["<c-d>"] = { vim.lsp.diagnostic.show_line_diagnostics, "" },
      ["R"] = { rename, "Rename current symbol under cursor", "IDE" },
      ["K"] = { vim.lsp.buf.hover, "Hover information about symbol under cursor", "IDE" },
      ["[d"] = { vim.lsp.diagnostic.goto_next, "Goto next diagnostic", "IDE" },
      ["]d"] = { vim.lsp.diagnostic.goto_prev, "Goto previous diagnostic", "IDE" },
    }
    local filetype = vim.api.nvim_buf_get_option(0, "filetype")
    if vim.tbl_contains(support_formatting, filetype) then
      vim.cmd [[ autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync() ]]
    end
    if client.resolved_capabilities.document_highlight then
      vim.cmd [[
        augroup lsp_document_highlight
          autocmd! * <buffer>
          autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
          autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
        augroup END
      ]]
    end
    if base then
      base()
    end
  end
end
local on_attach = make_on_attach(require("amirrezaask.telescope").on_attach)

lspconfig.gopls.setup {
  on_attach = on_attach,
}
lspconfig.rust_analyzer.setup {
  cmd = { "rustup", "run", "nightly", "rust-analyzer" },
  on_attach = function(client)
    on_attach(client)
    require("lsp_extensions").inlay_hints {}
  end,
}

lspconfig.tsserver.setup {
  on_attach = on_attach,
}

local sumneko_root = "/home/amirreza/.local/lua-language-server"
local sumneko_binary = sumneko_root .. "/bin/Linux/lua-language-server"

if has_nlua then
  nlua.setup(require "lspconfig", {
    on_attach = on_attach,
    cmd = {
      sumneko_binary,
      "-E",
      string.format("%s/main.lua", sumneko_root),
    },

    -- Include globals you want to tell the LSP are real :)
    globals = {
      "Color",
      "c",
      "Group",
      "g",
      "s",
      "vim",
      "Action",
      "describe",
      "it",
      "before_each",
      "after_each",
      "teardown",
      "pending",
      "clear",
      "awesome",
      "client",
    },
  })
end
lspconfig.jedi_language_server.setup { on_attach = on_attach }
lspconfig.clangd.setup { on_attach = on_attach }
lspconfig.intelephense.setup { on_attach = on_attach }

vim.lsp.handlers["textDocument/definition"] = function(_, _, result)
  if not result or vim.tbl_isempty(result) then
    print "[LSP] Could not find definition"
    return
  end
  P(result)
  if vim.tbl_islist(result) then
    vim.lsp.util.jump_to_location(result[1])
  else
    vim.lsp.util.jump_to_location(result)
  end
end

return M
