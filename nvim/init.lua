local modules = require "core.modules"
local ui = require "core.ui"

ui.colorscheme("sitruuna")

modules {
    "editor",
    "telescope",
    "completion",

    "git",

    "golang",
    "haskell",
    "elixir",
    "python",
    'purescript',
    "rust",
    "zig",
    "php",
    "lua",
    "cc",
    "yaml",
    "jai"
}
