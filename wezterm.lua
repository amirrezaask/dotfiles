local wezterm = require("wezterm")
local action = wezterm.action
local Multiple = action.Multiple
local SendKey = action.SendKey
local SendString = action.SendString


local config = {}

config.font_size = 16
config.font = wezterm.font_with_fallback {
    -- 'Monaspace Neon',
    -- 'Monaspace Xenon',
    -- 'Monaspace Krypton',
    -- 'Monaspace Argon',
    -- 'Monaspace Radon',
    -- 'MesloLGL Nerd Font Mono',
    -- 'Meslo',
    -- "Consolas",
    -- 'Cascadia Mono NF',
    -- 'Intel One Mono',
    'Jetbrains Mono',
}

config.window_padding = {
    left = 0,
    right = 0.0,
    top = 0.0,
    bottom = '0.0cell',
}

config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true


local tmux_enabled = true

local tmux_prefix = { key = 'a', mods = 'CTRL' }

-- local function tmux_command_action(command) -- BROKEN
--     return Multiple {
--         SendKey(tmux_prefix),
--         SendKey({ mods = "SHIFT", key = ';' }),
--         SendString(command),
--         SendKey({ key = 'Enter' })
--     }
-- end

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

config.keys = {
    { mods = 'CMD',       key = 't',          action = tmux_with_prefix_key_action('c') },                          -- Command+T new tmux window

    { mods = 'CMD|SHIFT', key = 'p',          action = tmux_with_prefix_key_action('s') },                          -- Command+Shift+P  switch session
    { mods = 'CMD',       key = 'o',          action = tmux_with_prefix_key_action({ key = 'w', mods = 'CTRL' }) }, -- Command+o  tmux-windowizer
    { mods = 'CMD|SHIFT', key = 'o',          action = tmux_with_prefix_key_action({ key = 's', mods = 'CTRL' }) }, -- Command+Shift+o  tmux-sessionizer

    { mods = 'CMD',       key = 'p',          action = tmux_with_prefix_key_action('p') },                          -- Command+P  previous window
    { mods = 'CMD',       key = 'n',          action = tmux_with_prefix_key_action('n') },                          -- Command+N  next window
    { mods = 'CMD',       key = 'LeftArrow',  action = tmux_with_prefix_key_action('p') },                          -- Command+<- previous window
    { mods = 'CMD',       key = 'RightArrow', action = tmux_with_prefix_key_action('n') },                          -- Command+-> next window

    { mods = 'CMD',       key = 'w',          action = tmux_with_prefix_key_action('x') },                          -- Command+W close window

    { mods = 'CMD',       key = 'd',          action = tmux_with_prefix_key_action('%') },                          -- Command+d split window vertically
    { mods = 'CMD|SHIFT', key = 'D',          action = tmux_with_prefix_key_action('"') },                          -- Command+Shift+d split window horizontally
}

config.adjust_window_size_when_changing_font_size = false

config.hide_tab_bar_if_only_one_tab = true

config.tab_max_width = 20

-- Return the concise name or icon of the running process for display
-- wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
--     local function get_display_cwd(tab)
--         local function get_cwd(tab)
--             return tab.active_pane.current_working_dir.file_path or ""
--         end
--         local function remove_abs_path(path) return path:gsub("(.*[/\\])(.*)", "%2") end
--         local current_dir = get_cwd(tab)
--         local HOME_DIR = string.format("file://%s", os.getenv("HOME"))
--         return current_dir == HOME_DIR and "~/" or remove_abs_path(current_dir)
--     end
--
--     local function get_process(tab)
--         if not tab.active_pane or tab.active_pane.foreground_process_name == "" then return "[?]" end
--         local function remove_abs_path(path) return path:gsub("(.*[/\\])(.*)", "%2") end
--         local process_name = remove_abs_path(tab.active_pane.foreground_process_name)
--         if process_name:find("kubectl") then process_name = "kubectl" end
--         if process_name == "zsh" then
--             return ""
--         end
--         return process_name
--     end
--
--
--
--     local title = get_display_cwd(tab)
--     local process = get_process(tab)
--
--     local tab_title = ''
--     if process ~= "" then
--         tab_title = string.format("%s %s", process, title)
--     else
--         tab_title = string.format("%s", title)
--     end
--
--     if tab.is_active then
--         return {
--             { Attribute = { Intensity = "Bold" } },
--             { Text = ' ' .. tab_title .. ' ' }
--         }
--     else
--         return ' ' .. tab_title .. ' '
--     end
-- end)
--
return config
