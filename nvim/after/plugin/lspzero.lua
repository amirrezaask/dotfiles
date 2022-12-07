if true then
  return
end

local ok, _ = require "lsp-zero"
if not ok then
  return
end

local lsp = require "lsp-zero"

lsp.preset "recommended"

lsp.nvim_workspace {
  library = vim.api.nvim_get_runtime_file("", true),
}

lsp.ensure_installed {
  "gopls",
  "rust_analyzer",
  "sumneko_lua",
}

lsp.on_attach(function(client, bufnr)
  local keymaps = require "core.keymaps"
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  keymaps.bind {
    n = {
      gd = { vim.lsp.buf.definition, desc = "Goto definition", buffer = bufnr },
      gi = { vim.lsp.buf.implementation, desc = "Goto implementations", buffer = bufnr },
      gr = { vim.lsp.buf.references, desc = "Goto references", buffer = bufnr },
      R = { vim.lsp.buf.rename, desc = "Rename symbol under cursor", buffer = bufnr },
      K = { vim.lsp.buf.hover, desc = "Hover docs under cursor", buffer = bufnr },

      ["<leader>lf"] = { vim.lsp.buf.format, desc = "format buffer", buffer = bufnr },

      ["<leader>ld"] = { vim.diagnostic.open_float, desc = "Show floating diagnostics", buffer = bufnr },
      ["[d"] = { vim.diagnostic.goto_prev, desc = "Goto previous diagnostic", buffer = bufnr },
      ["]d"] = { vim.diagnostic.goto_next, desc = "Goto next diagnostic", buffer = bufnr },

      ["<A-p>"] = { vim.diagnostic.goto_prev, desc = "Goto previous diagnostic", buffer = bufnr },
      ["<A-n>"] = { vim.diagnostic.goto_next, desc = "Goto next diagnostic", buffer = bufnr },

      ["C"] = { vim.lsp.buf.code_action, desc = "Code Actions", buffer = bufnr },

      ["<C-s>"] = { vim.lsp.buf.signature_help, desc = "Toggle Signature help", buffer = bufnr },
    },
    i = {
      ["<C-s>"] = { vim.lsp.buf.signature_help, desc = "Toggle Signature help", buffer = bufnr },
    },
  }
  local ok, signature = pcall(require, "lsp-Signature")
  if ok then
    signature.on_attach({}, bufnr)
  end
end)

lsp.setup()
