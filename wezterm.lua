local wezterm = require 'wezterm'

local config = {}
if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.font = wezterm.font_with_fallback { "Jetbrains Mono", "Fira Code", "Liberation Mono", }

config.use_fancy_tab_bar = false

config.tab_bar_at_bottom = true
config.tab_max_width = 35
-- config.color_scheme = 'DoomOne'

config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}
config.enable_scroll_bar = false

config.keys = {
  {
    mods = "CTRL|SHIFT",
    key = "k",
    action = wezterm.action.ActivateTabRelative(1),
  },
  {
    mods = "CTRL|SHIFT",
    key = "j",
    action = wezterm.action.ActivateTabRelative(-1),
  },
  {
    mods = "CTRL|SHIFT",
    key = "|",
    action = wezterm.action.SplitHorizontal,
  },
  {
    mods = "CTRL|SHIFT",
    key = "\"",
    action = wezterm.action.SplitVertical,
  },
  {
    mods = "CTRL|SHIFT",
    key = "LeftArrow",
    action = wezterm.action.ActivateTabRelative(-1)
  },
  {
    mods = "CTRL|SHIFT",
    key = "f",
    action = wezterm.action.ToggleFullScreen,
  },
  {
    mods = "CTRL|SHIFT",
    key = "RightArrow",
    action = wezterm.action.ActivateTabRelative(1)
  },

}

return config
