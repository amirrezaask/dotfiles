local wezterm = require "wezterm"
local config = wezterm.config_builder()
-- #1DA457

config.tab_bar_at_bottom = true

config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}
config.use_fancy_tab_bar = false

return config
