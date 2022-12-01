-- Core functionalities and utilities
require "core"

-- Now we set values we want.
plugins.fuzzy_finder = "telescope"

-- Colorscheme, check :Theme command for available options
plugins.colorscheme = "catppuccin"
plugins.transparent = false

-- Wether you want startup alpha dashboard or no.
plugins.alpha = {
  enabled = true,
  asciiart = [[

███    ██ ███████  ██████  ██    ██ ██ ███    ███ 
████   ██ ██      ██    ██ ██    ██ ██ ████  ████ 
██ ██  ██ █████   ██    ██ ██    ██ ██ ██ ████ ██ 
██  ██ ██ ██      ██    ██  ██  ██  ██ ██  ██  ██ 
██   ████ ███████  ██████    ████   ██ ██      ██ 
                                                  
                                                  
  ]],
}

-- Some language specific settings
-- language specific ones will override globals
-- by default I don't want auto format
-- but for specific languages i want them
langs = {
  autoformat = false,

  go = {
    autoformat = true,
  },

  lua = {
    autoformat = true,
  },
}

-- Loader
-- install all missing plugins
-- then load all your /lua/plugins/*.lua and /lua/langs/*.lua
require "core.loader"
