local wezterm = require("wezterm")
local config = {}

config.font_size = 16

config.font = wezterm.font_with_fallback {
	'MesloLGL Nerd Font Mono',
	'Meslo',
	"Consolas",
	'Cascadia Mono NF',
	'Intel One Mono',
	'Jetbrains Mono',
}

config.window_padding = {
	left = 0,
	right = 0,
	top = 0,
	bottom = 0,
}
local MOD = 'CTRL|SHIFT'

config.use_fancy_tab_bar = false

config.keys = {
	{ key = "t",          mods = MOD, action = wezterm.action.SpawnTab('CurrentPaneDomain') },
	{ key = "n",          mods = MOD, action = wezterm.action.SpawnTab('CurrentPaneDomain') },
	{ key = "j",          mods = MOD, action = wezterm.action.ActivateTabRelative(-1) },
	{ key = "k",          mods = MOD, action = wezterm.action.ActivateTabRelative(1) },
	{ key = "LeftArrow",  mods = MOD, action = wezterm.action.ActivateTabRelative(-1) },
	{ key = "RightArrow", mods = MOD, action = wezterm.action.ActivateTabRelative(1) },
	{ key = "(",          mods = MOD, action = wezterm.action.MoveTabRelative(-1) },
	{ key = ")",          mods = MOD, action = wezterm.action.MoveTabRelative(1) },
}

config.adjust_window_size_when_changing_font_size = false

config.hide_tab_bar_if_only_one_tab = true

config.tab_max_width = 20

return config
