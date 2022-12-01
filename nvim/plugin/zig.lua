use {
  "ziglang/zig.vim",
}

function configs.zig()
  require("nvim-treesitter.install").ensure_installed "zig"
  lsp.config("zls", {
    on_attach = lsp.on_attach,
  })
end
