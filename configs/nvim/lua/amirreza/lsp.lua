vim.pack.add {
  "https://github.com/neovim/nvim-lspconfig",
  "https://github.com/mason-org/mason.nvim",
  "https://github.com/mason-org/mason-lspconfig.nvim",
  "https://github.com/rachartier/tiny-inline-diagnostic.nvim",
}

require("mason").setup {}
require("mason-lspconfig").setup {
  ensure_installed = { "lua_ls", "gopls", "ts_ls" },
}

vim.lsp.config("lua_ls", {
  settings = {
    Lua = {
      diagnostics = { globals = { "vim" } },
      workspace = { library = vim.api.nvim_get_runtime_file("", true) },
    },
  },
})

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf, desc = "Open Floating Diagnostic" })
    vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf, desc = "Code Actions" })
  end,
})

require("tiny-inline-diagnostic").setup {
  preset = "powerline",
  options = {
    add_messages = {
      display_count = true,
      messages = true,
    },
    multilines = {
      always_show = true,
      enabled = true,
    },
  },
}
vim.diagnostic.config { virtual_text = false }
