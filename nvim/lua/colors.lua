local base16 = require 'base16'
local theme_names = base16.theme_names()
base16_position = 1
function CYCLE_THEME()
  base16_position = (base16_position % #theme_names) + 1
  print("Current theme => ", theme_names[base16_position])
  base16(base16.themes[theme_names[base16_position]], true)
end
base16(base16.themes["horizon-dark"], true)
vim.cmd [[ map <f3> <cmd> lua CYCLE_THEME()<CR> ]]

