--     ___              _                            ___         __
--    /   |  ____ ___  (_)____________  ____  ____ _/   |  _____/ /__
--   / /| | / __ `__ \/ / ___/ ___/ _ \/_  / / __ `/ /| | / ___/ //_/
--  / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ / ___ |(__  ) ,<
-- /_/  |_/_/ /_/ /_/_/_/  /_/   \___/ /___/\__,_/_/  |_/____/_/|_|

-- A simple note to people reading my configuration stuff
--
-- ./lua/plugins.lua
-- is where all 3rd party plugins are installed using `packer.nvim`
--
-- ./lua/amirrezaask/*.lua
-- is were I put my new plugins or extensions for existing plugins.
--
-- ./plugin/*
-- This is were I put neovim itself configuration like keymaps.
--
-- ./after/*
-- This is were I put configurations for 3rd party plugins I installed.

-- Extensions for neovim on vim metatable, most of them are WIP in neovim core and when they are merged I will remove them from this.
require "nvim"

-- Map leader to <Space>
vim.g.mapleader = " "

-- Set the colorscheme
vim.colorscheme "gruvbuddy"

-- Load plugins
require "plugins"
