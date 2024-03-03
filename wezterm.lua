local wezterm = require "wezterm"
local config = {}

config.font = wezterm.font_with_fallback {
  "Fira Code",
  "Jetbrains Mono",
  "Consolas",
  "Ubuntu Mono",
}
config.font_size = 16

config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}

config.use_fancy_tab_bar = false
config.bidi_enabled = true
config.tab_bar_at_bottom = true

-- config.window_background_opacity = 0.92

config.keys = {
  { key = 'j', mods = 'CTRL|SHIFT', action = wezterm.action.ActivateTabRelative(-1) },
  { key = 'k', mods = 'CTRL|SHIFT', action = wezterm.action.ActivateTabRelative(1) },
  { key = '(', mods = 'CTRL|SHIFT', action = wezterm.action.MoveTabRelative(-1) },
  { key = ')', mods = 'CTRL|SHIFT', action = wezterm.action.MoveTabRelative(1) },
}

config.adjust_window_size_when_changing_font_size = false

config.hide_tab_bar_if_only_one_tab = true

if wezterm.target_triple == 'x86_64-pc-windows-msvc' then -- if windows
  config.default_prog = { 'cmd.exe', '/k', '%cmder_root%/vendor/init.bat' }
end


return config
