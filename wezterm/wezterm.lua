local wezterm = require("wezterm")
local config = {}

config.font_size = 16

config.window_padding = {
	left = 0,
	right = 0,
	top = 0,
	bottom = 0,
}

config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true

config.keys = {
	{ key = "j",          mods = "CTRL|SHIFT", action = wezterm.action.ActivateTabRelative(-1) },
	{ key = "k",          mods = "CTRL|SHIFT", action = wezterm.action.ActivateTabRelative(1) },
	{ key = "LeftArrow",  mods = "CTRL|SHIFT", action = wezterm.action.ActivateTabRelative(-1) },
	{ key = "RightArrow", mods = "CTRL|SHIFT", action = wezterm.action.ActivateTabRelative(1) },
	{ key = "(",          mods = "CTRL|SHIFT", action = wezterm.action.MoveTabRelative(-1) },
	{ key = ")",          mods = "CTRL|SHIFT", action = wezterm.action.MoveTabRelative(1) },
}

config.adjust_window_size_when_changing_font_size = false

config.hide_tab_bar_if_only_one_tab = true

config.tab_max_width = 20

return config
