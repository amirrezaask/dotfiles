use {
  "glepnir/lspsaga.nvim",
  branch = "main",
}

function configs.lspsaga()
  require("lspsaga").init_lsp_saga {
    symbol_in_winbar = {
      enable = true,
    },
  }
end
