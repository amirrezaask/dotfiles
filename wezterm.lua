local wezterm = require "wezterm"
local config = {}

config.default_cwd = "W:\\"

config.font = wezterm.font_with_fallback {
    'JetBrains Mono',
    "Consolas"
}
config.font_size = 16

config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}

config.use_fancy_tab_bar = false
config.bidi_enabled = true
config.tab_bar_at_bottom = true

config.window_background_opacity = 0.9

config.keys = {
  { key = 'j', mods = 'CTRL|SHIFT', action = wezterm.action.ActivateTabRelative(-1) },
  { key = 'k', mods = 'CTRL|SHIFT', action = wezterm.action.ActivateTabRelative(1) },
}

return config
