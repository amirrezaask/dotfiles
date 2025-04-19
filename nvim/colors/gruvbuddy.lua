local base16 = require("base16")
local RGB = base16.rgb


local dark_base = '#282c34'


local colors = {                           -- Colors are insipired by @tjdevries ( https://github.com/tjdevries/colorbuddy.nvim )
    -- UI
    RGB.from_hex(dark_base),               -- base00
    RGB.from_hex(dark_base):lighten(0.1),  -- base01
    RGB.from_hex('#81A2BE'),               -- base02
    RGB.from_hex('#969896'),               -- base03
    RGB.from_hex('#373B41'),               -- base04
    RGB.from_hex('#e0e0e0'),               -- base05
    RGB.from_hex(dark_base):lighten(0.15), -- base06
    RGB.from_hex(dark_base):lighten(0.20), -- base07

    -- Syntax
    RGB.from_hex('#E0E0E0'), -- base08
    RGB.from_hex('#DE935F'), -- base09
    RGB.from_hex('#B294BB'), -- base0A
    RGB.from_hex('#99CC99'), -- base0B
    RGB.from_hex('#A992CD'), -- base0C
    RGB.from_hex('#F8FE7A'), -- base0D
    RGB.from_hex('#B294BB'), -- base0E
    RGB.from_hex('#E6B3B3'), -- base0F
}

base16.theme.new("gruvbuddy", colors):apply()

-- Fzflua
vim.api.nvim_set_hl(0, 'FzfLuaFzfMatch', { link = 'Number' })
vim.api.nvim_set_hl(0, 'FzfLuaFzfPrompt', { link = 'LineNr' })

-- Snacks
vim.api.nvim_set_hl(0, 'SnacksPickerListCursorLine', { link = 'CursorLine' })
vim.api.nvim_set_hl(0, 'SnacksPickerDir', { fg = colors[9]:to_hex() })
vim.api.nvim_set_hl(0, 'SnacksPickerMatch', { link = 'Number' })


vim.api.nvim_set_hl(0, 'TelescopeMatching', { link = 'Number' })
vim.api.nvim_set_hl(0, 'TelescopeSelection', { link = 'CursorLine' })
