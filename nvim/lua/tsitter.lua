local treesitter = require'nvim-treesitter.configs'
treesitter.setup {
  ensure_installed = {"go", "lua", "python"},
  highlight= {
    enable = true,
  },
  keymaps = {
    init_selection = "gnn",
    node_incremental = "grn",
    scope_incremental = "grc",
    node_decremental = "grm",
  },
  indent = {
    enable = true
  }
}
