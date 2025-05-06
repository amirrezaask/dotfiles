local wezterm = require "wezterm"
local config = wezterm.config_builder()

config.colors = {}

config.colors.background = "#111416"
config.colors.foreground = "#e0e0e0"
config.colors.cursor_bg = "#1DA457"
config.colors.selection_bg = "#81A2BE"
config.colors.selection_fg = "#5a7185"

config.tab_bar_at_bottom = false

config.window_padding = {
  left = 0,
  right = 0,
  top = 10,
  bottom = 1,
}

config.adjust_window_size_when_changing_font_size = false

config.use_fancy_tab_bar = false

config.max_fps = 75

config.tab_max_width = 24

config.window_decorations = "RESIZE" -- optional, hides title bar

-- config.font = wezterm.font
config.font = wezterm.font_with_fallback {
  { family = "TX-02", weight = "Medium" },
  { family = "Berkeley Mono", weight = "Medium" },
  { family = "IBM Plex Mono", weight = "Bold" },
  { family = "Jetbrains Mono", weight = "Medium" },
  { family = "Fira Code", weight = "Medium" },
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

wezterm.on("format-tab-title", function(tab, _, _, _, _, _)
  local cwd_uri = tab.active_pane.current_working_dir
  if cwd_uri then
    local cwd = cwd_uri.file_path -- On Unix
    cwd = cwd:gsub("/$", "") -- Remove trailing slash
    local project_name = cwd:match("^.+/(.+)$") or cwd
    return string.format(" %s: %s ", tab.tab_index, project_name)
  end
  return "No CWD"
end)

return config
