use {
  "glepnir/lspsaga.nvim",
  branch = "main",
}

function configs.lspsaga()
  require("lspsaga").init_lsp_saga {
    code_action_lightbulb = {
      enable = false,
    },
    symbol_in_winbar = {
      enable = true,
    },
  }
end
