require "options" -- Vim options
require "keymaps" -- Vim keymaps

-- Installing lazy package manager
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system {
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    }
end
vim.opt.rtp:prepend(lazypath)

TRANSPARENT = true

-- Installing plugins and configuring them
-- see https://github.com/folke/lazy.nvim#-structuring-your-plugins
require("lazy").setup("plugins", { -- Loading all plugin specs in lua/plugins/*.lua
    change_detection = { notify = false },
})

-- Setting the colorscheme
vim.cmd.colorscheme "tokyonight-night"
