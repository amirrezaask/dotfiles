function configs.haskell()
  require("nvim-treesitter.install").ensure_installed "haskell"
  lsp.config("hls", {
    on_attach = lsp.on_attach,
  })
end
