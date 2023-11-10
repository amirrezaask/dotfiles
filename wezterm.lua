local wezterm = require'wezterm'

local cfg = {}
if wezterm.config_builder then
  cfg = wezterm.config_builder()
end


cfg.font = wezterm.font "Jetbrains Mono"

cfg.colors = {}
cfg.colors.background = '#111111'

cfg.tab_bar_at_bottom = true

return cfg

