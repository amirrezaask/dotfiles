-- My neovim standard lib :)

require "nvim"

vim.g.mapleader = " "

-- Theme
-- require "colors.gruvbuddy"
require("base16.themes").norcalli:apply()

-- Load plugins and configuration
require "plugins"
