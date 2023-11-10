local wezterm = require'wezterm'

local cfg = {}
if wezterm.config_builder then
  cfg = wezterm.config_builder()
end


cfg.font = wezterm.font_with_fallback { "Jetbrains Mono", "Fira Code", "Liberation Mono" }

cfg.colors = {}
cfg.use_fancy_tab_bar = false

cfg.tab_bar_at_bottom = true
cfg.tab_max_width = 35

return cfg

