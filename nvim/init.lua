--     ___              _                            ___         __  
--    /   |  ____ ___  (_)____________  ____  ____ _/   |  _____/ /__
--   / /| | / __ `__ \/ / ___/ ___/ _ \/_  / / __ `/ /| | / ___/ //_/
--  / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ / ___ |(__  ) ,<   
-- /_/  |_/_/ /_/ /_/_/_/  /_/   \___/ /___/\__,_/_/  |_/____/_/|_|  
-- AmirrezaAsk neovim configuration
-- First two modules are pretty obvious
-- requiring lazy will give us access to lazy package manager api
-- and then we load all submodules in plugins namespace (lua/plugins/*.lua)

require"options"
require"keymaps"
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

function get_path_sep()
	if vim.fn.has("win32") == 1 then
		return "\\"
	else
		return "/"
	end
end

require"lazy".setup("plugins")

vim.cmd.colorscheme "catppuccin-mocha"
