require('blame').setup {
  always = false,
  prefix = ''
}

vim.nmap {
  [',b'] = require('blame').blame,
  [',c'] = require('blame').clear
}
