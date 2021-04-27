local base16 = require('base16')

local nvim = require('nvim')

function SetBas16Colorscheme(name)
  base16(require('base16').themes[name], true)
end

base16_position = 1
function cycle_theme()
  local theme_names = base16.theme_names()
  base16_position = (base16_position % #theme_names) + 1
  print(theme_names[base16_position])
  base16(base16.themes[theme_names[base16_position]], true)
end

function ColorPicker()
  _PICKER_ASHKAN_KIANI_COPYRIGHT_2020_LONG_NAME_HERE_ = nil
  require('colorizer').color_picker_on_cursor()
end

vim.cmd(string.format([[command! -nargs=1 Base16Editor lua require'base16.editor'.open(require'base16'.themes["<args>"])]]))
-- nvim.command('Base16Editor', [[lua require'base16.editor'.open(require'base16'.themes["<args>"])]], 1)
-- nvim.command('Base16Cycle', cycle_theme)
