local ctrltab = require('ctrltab')

vim.map {
  ['n `'] = ctrltab.jump_to_last,
  ['<M-`>'] = ctrltab.telescope_switcher,
}
