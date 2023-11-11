local wezterm = require 'wezterm'

local config = {}
if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.font = wezterm.font_with_fallback { "Jetbrains Mono", "Fira Code", "Liberation Mono", }

config.use_fancy_tab_bar = false

config.tab_bar_at_bottom = true
config.tab_max_width = 35
config.color_scheme = 'Gruvbox dark, hard (base16)'


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
