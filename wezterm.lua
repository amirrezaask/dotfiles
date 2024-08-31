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
        'Cascadia Mono NF',
        'Jetbrains Mono',
        'Meslo',
        "Consolas",
    },
    window_padding = {
        left = 5,
        right = 5.0,
        top = 5.0,
        bottom = 5.0,
    },
    line_height = 1.3,
    use_fancy_tab_bar = false,
    tab_bar_at_bottom = true,

    window_background_opacity = 1.0,
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



-- macOS keys
wt_key({ mods = 'CMD', key = 'p', action = action.ActivateTabRelative(-1) })
wt_key({ mods = 'CMD', key = 'n', action = action.ActivateTabRelative(1) })
wt_key({ mods = 'CMD|SHIFT', key = '9', action = action.MoveTabRelative(-1) })
wt_key({ mods = 'CMD|SHIFT', key = '0', action = action.MoveTabRelative(-1) })
wt_key({ mods = 'CMD', key = 't', action = action.SpawnTab('CurrentPaneDomain') })
wt_key({ mods = 'CMD', key = 'w', action = action.CloseCurrentPane({ confirm = false }) })
wt_key({ mods = 'CMD', key = 'd', action = action.SplitHorizontal({ domain = 'CurrentPaneDomain' }) })
wt_key({ mods = 'CMD|SHIFT', key = 'd', action = action.SplitVertical({ domain = 'CurrentPaneDomain' }) })
wt_key({ key = 'r', mods = 'CMD|SHIFT', action = wezterm.action.ReloadConfiguration, })
wt_key({ key = 'j', mods = 'CMD', action = wezterm.action.ActivatePaneDirection "Prev", })
wt_key({ key = 'k', mods = 'CMD', action = wezterm.action.ActivatePaneDirection "Next", })


local light_theme = 'GruvboxLight'
local dark_theme = 'GruvboxDarkHard'
local color_mode = 'dark'
config.color_scheme = dark_theme

local function toggle_color_mode(win, _)
    local cfg = win:get_config_overrides() or {}
    if color_mode == 'light' then
        color_mode = 'dark'
        cfg.color_scheme = dark_theme
    else
        color_mode = 'light'
        cfg.color_scheme = light_theme
    end

    win:set_config_overrides(cfg)
end


wt_key({
    key = '\r',
    mods = 'CMD',
    action = wezterm.action.Multiple {
        wezterm.action_callback(toggle_color_mode),
        wezterm.action.SendKey({ key = 't', mods = 'ALT' })
    }
})


return config
