--     ___              _                            ___         __
--    /   |  ____ ___  (_)____________  ____  ____ _/   |  _____/ /__
--   / /| | / __ `__ \/ / ___/ ___/ _ \/_  / / __ `/ /| | / ___/ //_/
--  / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ / ___ |(__  ) ,<
-- /_/  |_/_/ /_/ /_/_/_/  /_/   \___/ /___/\__,_/_/  |_/____/_/|_|
-- AmirrezaAsk neovim configuration
-- First two modules are pretty obvious
-- requiring lazy will give us access to lazy package manager api
-- and then we load all submodules in plugins namespace (lua/plugins/*.lua)

require "options"
require "keymaps"

-- lazy installation code
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

TRANSPARENT = false
vim.opt.rtp:prepend(lazypath)
require("lazy").setup ("plugins", {
  change_detection = {
    enabled = true,
    notify = false,
  }
})

local light_theme = "rose-pine-dawn"
local dark_theme =  "rose-pine"

vim.cmd(string.format([[ command! Light colorscheme %s ]], light_theme))
vim.cmd(string.format([[ command! Dark colorscheme %s ]], dark_theme))

vim.cmd.Light()

