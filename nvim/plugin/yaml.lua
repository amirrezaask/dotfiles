use { "cuducos/yaml.nvim" }
function configs.yaml()
  require("nvim-treesitter.install").ensure_installed "yaml"
  lsp.config("yamlls", {
    on_attach = lsp.on_attach,
  })
end
