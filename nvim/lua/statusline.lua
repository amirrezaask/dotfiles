local el = require('el')
local extensions = require('el.extensions')
local subscribe = require('el.subscribe')
local builtin = require('el.builtin')
local sections = require('el.sections')


local function setup()
   el.setup{} 
end

return {
    setup = setup
}
