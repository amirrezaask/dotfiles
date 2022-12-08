if not require("core.utils").has_plugins "lsp-zero" then
  return
end

local lsp = require "lsp-zero"

lsp.preset "recommended"

lsp.set_preferences {
  suggest_lsp_servers = false,
}

lsp.nvim_workspace {
  library = vim.api.nvim_get_runtime_file("", true),
}

lsp.ensure_installed {
  "gopls",
  "rust_analyzer",
  "sumneko_lua",
  "clangd",
  "elixirls",
  "hls",
  "jsonls",
  "intelephense",
  "jedi_language_server",
  "zls",
  "yamlls",
}

lsp.on_attach(function(_, bufnr)
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

      ["<leader>dl"] = { vim.diagnostic.open_float, desc = "Show floating diagnostics", buffer = bufnr },
      ["<leader>dp"] = { vim.diagnostic.goto_prev, desc = "Goto previous diagnostic", buffer = bufnr },
      ["<leader>dn"] = { vim.diagnostic.goto_next, desc = "Goto next diagnostic", buffer = bufnr },

      ["C"] = { vim.lsp.buf.code_action, desc = "Code Actions", buffer = bufnr },
      ["<C-s>"] = { vim.lsp.buf.signature_help, desc = "Toggle Signature help", buffer = bufnr },
    },
    i = {
      ["<C-s>"] = { vim.lsp.buf.signature_help, desc = "Toggle Signature help", buffer = bufnr },
    },
  }
end)

local cmp = require "cmp"
local cmp_select = { behavior = cmp.SelectBehavior.Select }

lsp.setup_nvim_cmp {
  mapping = lsp.defaults.cmp_mappings {
    ["<C-p>"] = cmp.mapping.select_prev_item(cmp_select),
    ["<C-n>"] = cmp.mapping.select_next_item(cmp_select),
  },
}

lsp.configure("sumneko_lua", {
  settings = {
    Lua = {
      diagnostics = {
        globals = { "vim" },
      },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true),
        checkThirdParty = false,
      },
      telemetry = {
        enable = false,
      },
    },
  },
})

lsp.configure("jsonls", {
  settings = {
    json = {
      schemas = require("schemastore").json.schemas(),
      validate = { enable = true },
    },
  },
})

lsp.setup()

local null_opts = lsp.build_options("null-ls", {})

require("null-ls").setup {
  on_attach = null_opts.on_attach,
  sources = {
    require("null-ls").builtins.code_actions.gitsigns,
    require("null-ls").builtins.diagnostics.gitlint,
    require("null-ls").builtins.diagnostics.golangci_lint,
    require("null-ls").builtins.diagnostics.trail_space.with { disabled_filetypes = { "NvimTree" } },
    require("null-ls").builtins.formatting.stylua,
    require("null-ls").builtins.formatting.goimports,
  },
}

Mason = {}
local install_path = require("mason-core.path").concat { vim.fn.stdpath "data", "mason" }

local function MasonInstall(to_install)
  local function has(name)
    return vim.fn.filereadable(require("mason-core.path").concat { install_path, "bin", name }) == 1
  end

  local missing = {}

  for _, name in pairs(to_install) do
    if not has(name) then
      table.insert(missing, name)
    end
  end
  require("mason.api.command").MasonInstall(missing)
end

MasonInstall { "gitlint", "stylua", "golangci-lint", "goimports", "gofumpt", "yamlfmt" }

vim.diagnostic.config {
  signs = false,
}
