local wezterm = require("wezterm")
local config = {}

config.font_size = 16
config.font = wezterm.font_with_fallback {
	'Cascadia Mono NF',
	'Jetbrains Mono',
	'Meslo',
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

config.window_background_opacity = 0.95

return config
