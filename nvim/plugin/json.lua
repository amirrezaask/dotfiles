use {
  "b0o/schemastore.nvim",
}

function configs.json()
  lsp.config("jsonls", {
    settings = {
      json = {
        schemas = require("schemastore").json.schemas(),
        validate = { enable = true },
      },
    },
  })

  require("nvim-treesitter.install").ensure_installed "json"
end
