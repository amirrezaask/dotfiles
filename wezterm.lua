local wezterm = require("wezterm")
local config = {}

config.font_size = 16

config.colors = {
    cursor_bg = "#47FF9C",
    cursor_border = "#47FF9C",
    cursor_fg = "#011423",
}

config.font = wezterm.font_with_fallback {
    'Monaspace Neon',
    -- 'Monaspace Xenon',
    -- 'Monaspace Krypton',
    -- 'Monaspace Argon',
    -- 'Monaspace Radon',
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
config.tab_bar_at_bottom = true

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
