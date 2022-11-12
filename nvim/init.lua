local ui = require"core.ui"
local modules = require"core.modules"

-- Set colorscheme
ui.colorscheme("tokyonight-night")

modules {
    "basics",
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
