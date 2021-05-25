local hl = require('palette').highlight
if vim.g.transparent then
  hl:apply { { "Normal", "LineNr", "CursorLineNR", "SignColumn" }, bg='none' }
end
