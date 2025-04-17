require("nvim-treesitter.configs").setup {
    ensure_installed = { "lua", "go", "gomod", "php" },
    highlight = { enable = true },
    sync_install = false,
    auto_install = true,
    ignore_install = {},
    modules = {}
}
