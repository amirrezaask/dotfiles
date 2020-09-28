local el = require('el')
local extensions = require('el.extensions')
local subscribe = require('el.subscribe')
local builtin = require('el.builtin')
local sections = require('el.sections')


local function setup_express_line()
   el.setup{}
end

local function setup_colorscheme()
    require'colorbuddy'.colorscheme('gruvbuddy')
end

setup_colorscheme()


if vim.api.nvim_get_var('enable_express_line') then setup_express_line() end
