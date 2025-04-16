return {
    cmd = { "lua-language-server" },
    filetypes = { "lua" },
    root_markers = { ".git" },
    settings = {
        Lua = {
            workspace = {
                userThirdParty = { os.getenv("HOME") .. ".local/share/LuaAddons" },
                checkThirdParty = "Apply"
            },
            diagnostics = {
                globals = { "vim" }
            }
        }
    },
}
