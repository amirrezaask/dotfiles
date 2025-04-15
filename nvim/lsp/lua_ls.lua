return {
    cmd = { "lua-language-server" },
    filetypes = { "lua" },
    root_markers = { ".git" },
    settings = { Lua = { diagnostics = { globals = { "vim" } } } },
}
