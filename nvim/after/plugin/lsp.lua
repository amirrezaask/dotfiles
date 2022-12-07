local function has_plugins(plugins)
  for _, name in ipairs(plugins) do
    local ok, _ = pcall(require, name)
    if not ok then
      return false
    end
  end

  return true
end

if not has_plugins { "mason", "mason-lspconfig", "cmp", "null-ls", "lspconfig" } then
  return
end

local Mason = {}
Mason.install_path = require("mason-core.path").concat { vim.fn.stdpath "data", "mason" }

require("mason").setup {
  install_root_dir = Mason.install_path,
}

require("mason-lspconfig").setup {
  automatic_installation = true,
}

function Mason.has(name)
  return vim.fn.filereadable(require("mason-core.path").concat { Mason.install_path, "bin", name }) == 1
end

function Mason.install(to_install)
  local missing = {}

  for _, name in pairs(to_install) do
    if not Mason.has(name) then
      table.insert(missing, name)
    end
  end
  require("mason.api.command").MasonInstall(missing)
end

local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview

function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
  opts = opts or {}
  opts.border = "rounded"
  return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

local function on_attach(_, bufnr)
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
end

vim.api.nvim_create_user_command("Format", function()
  vim.lsp.buf.format()
end, {})

local servers = {
  "clangd",
  "elixirls",
  "gopls",
  "hls",
  {
    "jsonls",
    {
      settings = {
        json = {
          schemas = require("schemastore").json.schemas(),
          validate = { enable = true },
        },
      },
    },
  },
  {
    "sumneko_lua",
    {
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
    },
  },
  "intelephense",
  "purescriptls",
  "jedi_language_server",
  "rust_analyzer",
  "zls",
}

for _, srv in ipairs(servers) do
  if type(srv) == "string" then
    require("lspconfig")[srv].setup {
      on_attach = on_attach,
    }
  end

  if type(srv) == "table" then
    require("lspconfig")[srv[1]].setup(vim.tbl_extend("keep", srv[2], {
      on_attach = on_attach,
    }))
  end
end

local cmp = require "cmp"

cmp.setup {
  snippet = {
    expand = function(args)
      require("luasnip").lsp_expand(args.body)
    end,
  },
  formatting = {
    format = require("lspkind").cmp_format {
      mode = "symbol", -- show only symbol annotations
      maxwidth = 50, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)
      ellipsis_char = "...", -- when popup menu exceed maxwidth, the truncated part would show ellipsis_char instead (must define maxwidth first)
    },
  },
  mapping = {
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.close(),
    ["<CR>"] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Insert,
      select = false,
    },
    ["<Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end,
    ["<S-Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end,
  },

  sources = {
    { name = "buffer" },
    { name = "luasnip" },
    { name = "nvim_lsp" },
    { name = "path" },
    { name = "nvim_lua" },
  },
}

require("null-ls").setup {
  sources = {
    require("null-ls").builtins.code_actions.gitsigns,
    require("null-ls").builtins.diagnostics.gitlint,
    require("null-ls").builtins.diagnostics.golangci_lint,
    require("null-ls").builtins.diagnostics.trail_space.with { disabled_filetypes = { "NvimTree" } },
    require("null-ls").builtins.formatting.stylua,
    require("null-ls").builtins.formatting.goimports,
  },
}

Mason.install { "gitlint", "stylua", "golangci-lint", "goimports" }
