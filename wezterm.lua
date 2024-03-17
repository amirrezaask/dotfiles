local wezterm = require("wezterm")
local config = {}

config.font = wezterm.font_with_fallback({
	"Comic Mono",
	"Hermit",
	"JetBrainsMono Nerd Font Mono",
	"CaskaydiaMono Nerd Font",
	"Fira Code",
	"Consolas",
	"Ubuntu Mono",
})
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
	{ key = "j", mods = "CTRL|SHIFT", action = wezterm.action.ActivateTabRelative(-1) },
	{ key = "k", mods = "CTRL|SHIFT", action = wezterm.action.ActivateTabRelative(1) },
	{ key = "(", mods = "CTRL|SHIFT", action = wezterm.action.MoveTabRelative(-1) },
	{ key = ")", mods = "CTRL|SHIFT", action = wezterm.action.MoveTabRelative(1) },
}

config.adjust_window_size_when_changing_font_size = false

config.hide_tab_bar_if_only_one_tab = true

if wezterm.target_triple == "x86_64-pc-windows-msvc" then -- if windows
	config.default_prog = { [[C:\Program Files\PowerShell\7\pwsh.exe]] }
	config.default_cwd = "C:\\w"
end

config.tab_max_width = 20

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
	local path = tab.active_pane.current_working_dir.file_path
	local title = path:gsub("^([\\/])", "")
	title = title:gsub("C:/Users/amirreza", "~")
	title = title:gsub("C:/w", "w")
	title = string.format("%d: %s", tab.tab_index, title)
	return {
		{ Text = title },
	}
end)

return config
