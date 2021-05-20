lua << EOF
require('amirrezaask.listchars'):update {
    ['lead']='·',
    ['trail']='*',
    eol = '↲',
    tab = '-> ',
    extends = '<',
    precedes = '>',
    conceal = '┊',
    nbsp = '␣',
  }
vim.autocmd { 'BufLeave', '<buffer>', function() require('amirrezaask.listchars'):update() end }
EOF
