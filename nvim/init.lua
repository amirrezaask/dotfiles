-- Initialize Lazy.nvim package manager
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
    if vim.v.shell_error ~= 0 then
        vim.api.nvim_echo({
            { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
            { out,                            "WarningMsg" },
            { "\nPress any key to exit..." },
        }, true, {})
        vim.fn.getchar()
        os.exit(1)
    end
end

vim.o.rtp = vim.o.rtp .. "," .. lazypath

vim.g.mapleader = " " -- We need to have this in case plugins use leader mapping correctly happen.

require("lazy").setup({ import = "plugins.spec" }, {
    change_detection = {
        notify = false,
    },
})

function Transparent()
    vim.cmd [[
        hi Normal guibg=none
        hi NormalFloat guibg=none
        hi LineNr guibg=none
        hi SignColumn guibg=none
        hi WinBorder guibg=none
    ]]
end

vim.cmd.colorscheme(vim.env.NVIM_COLORSCHEME or "gruvbuddy")
-- Transparent()
