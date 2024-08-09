local wezterm = require("wezterm")
local config = {}

config.font_size = 16

-- Coolnight from https://github.com/josean-dev/dev-environment-files/blob/main/.wezterm.lua
config.colors = {
	foreground = "#CBE0F0",
	background = "#011423",
	cursor_bg = "#47FF9C",
	cursor_border = "#47FF9C",
	cursor_fg = "#011423",
	selection_bg = "#033259",
	selection_fg = "#CBE0F0",
	ansi = { "#214969", "#E52E2E", "#44FFB1", "#FFE073", "#0FC5ED", "#a277ff", "#24EAF7", "#24EAF7" },
	brights = { "#214969", "#E52E2E", "#44FFB1", "#FFE073", "#A277FF", "#a277ff", "#24EAF7", "#24EAF7" },
}

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

config.enable_tab_bar = false
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true

config.keys = {
	{ key = "t",          mods = MOD, action = wezterm.action.SpawnTab('CurrentPaneDomain') },
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
