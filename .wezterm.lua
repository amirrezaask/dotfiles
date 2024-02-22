local wezterm = require "wezterm"
local config = {}

config.default_cwd = "W:\\"

config.font = wezterm.font_with_fallback {
    "Consolas"
}
config.font_size = 16

config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}


config.bidi_enabled = true
-- config.default_prog = { 'cmd.exe', '/k', '%cmder_root%/vendor/init.bat' }
config.default_prog = { 'powershell.exe' }
config.tab_bar_at_bottom = true

return config
