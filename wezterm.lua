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



-- Remove all path components and return only the last value

-- Return the pretty path of the tab's current working directory
--
local function get_display_cwd(tab)
    local function get_cwd(tab)
        return tab.active_pane.current_working_dir.file_path or ""
    end
    local function remove_abs_path(path) return path:gsub("(.*[/\\])(.*)", "%2") end
    local current_dir = get_cwd(tab)
    local HOME_DIR = string.format("file://%s", os.getenv("HOME"))
    return current_dir == HOME_DIR and "~/" or remove_abs_path(current_dir)
end

-- Return the concise name or icon of the running process for display
local function get_process(tab)
    if not tab.active_pane or tab.active_pane.foreground_process_name == "" then return "[?]" end
    local function remove_abs_path(path) return path:gsub("(.*[/\\])(.*)", "%2") end
    local process_name = remove_abs_path(tab.active_pane.foreground_process_name)
    if process_name:find("kubectl") then process_name = "kubectl" end
    if process_name == "zsh" then
        return ""
    end
    return process_name
end


wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
    local title = get_display_cwd(tab)
    local process = get_process(tab)

    local tab_title = ''
    if process ~= "" then
        tab_title = string.format("%s %s", process, title)
    else
        tab_title = string.format("%s", title)
    end

    if tab.is_active then
        return {
            { Attribute = { Intensity = "Bold" } },
            { Text = ' ' .. tab_title .. ' ' }
        }
    else
        return ' ' .. tab_title .. ' '
    end
end)

return config
