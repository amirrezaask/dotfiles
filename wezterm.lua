local wezterm = require 'wezterm'

local cfg = {}
if wezterm.config_builder then
  cfg = wezterm.config_builder()
end

cfg.color_scheme = 'Gruvbox dark, hard (base16)'

cfg.font = wezterm.font_with_fallback { "Jetbrains Mono", "Fira Code", "Liberation Mono", }

cfg.use_fancy_tab_bar = false

cfg.tab_bar_at_bottom = true
cfg.tab_max_width = 35


cfg.keys = {
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

return cfg
