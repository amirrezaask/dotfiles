local wezterm = require("wezterm")
local action = wezterm.action
local Multiple = action.Multiple
local SendKey = action.SendKey

local config = {
    adjust_window_size_when_changing_font_size = false,
    hide_tab_bar_if_only_one_tab = true,
    tab_max_width = 20,
    font_size = 16,
    font = wezterm.font_with_fallback {
        'JetBrainsMono Nerd Font Mono',
        'Jetbrains Mono',
        'Cascadia Mono NF',
        'Meslo',
        "Consolas",
    },
    window_padding = {
        left = 0,
        right = 0.0,
        top = 0.0,
        bottom = '0.0cell',
    },
    use_fancy_tab_bar = false,
    tab_bar_at_bottom = true,

    window_background_opacity = 0.75
    -- color_scheme = 'Catppuccin Macchiato',
}
local function wt_key(spec)
    if config.keys == nil then config.keys = {} end
    table.insert(config.keys, spec)
end


local TMUX_ENABLED = false
if TMUX_ENABLED then
    local tmux_prefix = { key = 'a', mods = 'CTRL' }
    local function tmux_with_prefix_key_action(tmux_key)
        if type(tmux_key) == "string" then
            return Multiple {
                SendKey(tmux_prefix),
                SendKey({ key = tmux_key })
            }
        elseif type(tmux_key) == "table" then
            return Multiple {
                SendKey(tmux_prefix),
                SendKey(tmux_key)
            }
        end
    end
    wt_key({ mods = 'CMD', key = 't', action = tmux_with_prefix_key_action('c') })                                -- Command+T new tmux window
    wt_key({ mods = 'CMD|SHIFT', key = 'p', action = tmux_with_prefix_key_action('s') })                          -- Command+Shift+P  switch session
    wt_key({ mods = 'CMD', key = 'o', action = tmux_with_prefix_key_action({ key = 'w', mods = 'CTRL' }) })       -- Command+o  tmux-windowizer
    wt_key({ mods = 'CMD|SHIFT', key = 'o', action = tmux_with_prefix_key_action({ key = 's', mods = 'CTRL' }) }) -- Command+Shift+o  tmux-sessionizer
    wt_key({ mods = 'CMD', key = 'p', action = tmux_with_prefix_key_action('p') })                                -- Command+P  previous window
    wt_key({ mods = 'CMD', key = 'n', action = tmux_with_prefix_key_action('n') })                                -- Command+N  next window
    wt_key({ mods = 'CMD', key = 'LeftArrow', action = tmux_with_prefix_key_action('p') })                        -- Command+<- previous window
    wt_key({ mods = 'CMD', key = 'RightArrow', action = tmux_with_prefix_key_action('n') })                       -- Command+-> next window
    wt_key({ mods = 'CMD', key = 'w', action = tmux_with_prefix_key_action('x') })                                -- Command+W close window
    wt_key({ mods = 'CMD', key = 'd', action = tmux_with_prefix_key_action('%') })                                -- Command+d split window vertically
    wt_key({ mods = 'CMD|SHIFT', key = 'D', action = tmux_with_prefix_key_action('"') })                          -- Command+Shift+d split window horizontally
end


wt_key({ mods = 'CMD', key = 'p', action = action.ActivateTabRelative(-1) })
wt_key({ mods = 'CMD', key = 'n', action = action.ActivateTabRelative(1) })
wt_key({ mods = 'CMD', key = 't', action = action.SpawnTab('CurrentPaneDomain') })
wt_key({ mods = 'CMD', key = 'w', action = action.CloseCurrentTab({ confirm = false }) })
wt_key({ mods = 'CMD', key = 'd', action = action.SplitHorizontal({ domain = 'CurrentPaneDomain' }) })
wt_key({ mods = 'CMD|SHIFT', key = 'd', action = action.SplitVertical({ domain = 'CurrentPaneDomain' }) })
wt_key({ key = 'r', mods = 'CMD|SHIFT', action = wezterm.action.ReloadConfiguration, })

return config
