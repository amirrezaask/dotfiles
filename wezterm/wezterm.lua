local wezterm = require("wezterm")
local config = {}

config.font_size = 16
config.font = wezterm.font_with_fallback {
	'MesloLGL Nerd Font Mono',
	'Cascadia Mono NF',
	'Jetbrains Mono',
	'Meslo',
}

-- Coolnight theme
config.colors = {
	foreground = "#CBE0F0",
	background = "#011423",
	cursor_bg = "#47FF9C",
	cursor_border = "#47FF9C",
	cursor_fg = "#011423",
	selection_bg = "#706b4e",
	selection_fg = "#f3d9c4",
	ansi = { "#214969", "#E52E2E", "#44FFB1", "#FFE073", "#0FC5ED", "#a277ff", "#24EAF7", "#24EAF7" },
	brights = { "#214969", "#E52E2E", "#44FFB1", "#FFE073", "#A277FF", "#a277ff", "#24EAF7", "#24EAF7" },
}

config.window_padding = {
	left = 0,
	right = 0,
	top = 0,
	bottom = 0,
}
local is_linux = function()
	return wezterm.target_triple:find("linux") ~= nil
end

local is_darwin = function()
	return wezterm.target_triple:find("darwin") ~= nil
end

local MOD = 'CTRL|SHIFT'

if is_darwin() then
	MOD = 'CMD|SHIFT'
end

config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true

config.keys = {
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

-- config.window_background_opacity = 0.95

return config
