local base16 = require('base16')

function SetBas16Colorscheme(name)
  base16(require('base16').themes[name], true)
end

vim.cmd(string.format([[command! -nargs=1 Base16Editor lua require'base16.editor'.open(require'base16'.themes["<args>"])]]))
