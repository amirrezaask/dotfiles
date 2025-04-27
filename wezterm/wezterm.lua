local wezterm = require "wezterm"
local config = wezterm.config_builder()

config.colors = {}

config.colors.background = "#111416"

config.tab_bar_at_bottom = true

config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}
config.use_fancy_tab_bar = false

config.tab_max_width = 24

config.font = wezterm.font_with_fallback {
  "TX-02",
  "Berkeley Mono",
  "IBM Plex Mono",
  "Jetbrains Mono",
  "Fira Code",
}

config.font_size = 12

config.keys = {
  -- Tab navigation
  { key = "n", mods = "CMD", action = wezterm.action.ActivateTabRelative(1) },
  { key = "p", mods = "CMD", action = wezterm.action.ActivateTabRelative(-1) },

  -- Pane navigation
  { key = "h", mods = "CMD", action = wezterm.action.ActivatePaneDirection("Left") },
  { key = "j", mods = "CMD", action = wezterm.action.ActivatePaneDirection("Down") },
  { key = "k", mods = "CMD", action = wezterm.action.ActivatePaneDirection("Up") },
  { key = "l", mods = "CMD", action = wezterm.action.ActivatePaneDirection("Right") },

  -- Toggle pane zoom
  { key = "f", mods = "CMD", action = wezterm.action.TogglePaneZoomState },

  -- Split panes
  { key = "d", mods = "CMD", action = wezterm.action.SplitHorizontal { domain = "CurrentPaneDomain" } },
  { key = "d", mods = "CMD|SHIFT", action = wezterm.action.SplitVertical { domain = "CurrentPaneDomain" } },

  { key = "w", mods = "CMD", action = wezterm.action.CloseCurrentPane { confirm = false } },
}

return config
