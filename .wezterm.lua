local wezterm = require "wezterm"
local config = {}

config.default_cwd = "W:\\"

config.font = wezterm.font_with_fallback {
    "Consolas"
}
config.font_size = 16

config.window_padding = {
  left = 1,
  right = 1,
  top = 0,
  bottom = 0,
}

config.default_prog = { 'cmd.exe', '/k', '%cmder_root%/vendor/init.bat' }

return config
