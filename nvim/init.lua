-- My neovim standard lib :)

require "nvim"

vim.g.mapleader = " "

-- Theme
-- require "colors.gruvbuddy"
-- require("base16.themes").norcalli:apply()
-- vim.c.colorscheme "gruvbox"
vim.c.colorscheme "cyberpunk"
vim.cmd [[ hi Normal guibg=none ]]

-- Load plugins and configuration
require "plugins"
