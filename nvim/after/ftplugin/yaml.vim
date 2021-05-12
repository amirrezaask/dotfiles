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
vim.cmd [[ autocmd BufLeave <buffer> lua require('amirrezaask.listchars'):update() ]]
EOF
