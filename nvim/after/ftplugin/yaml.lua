require('plugin.listchars'):update {
    ['lead']='·',
    ['trail']='*',
    eol = '↲',
    tab = '-> ',
    extends = '<',
    precedes = '>',
    conceal = '┊',
    nbsp = '␣',
  }
vim.autocmd { 'BufLeave', '<buffer>', function() require('plugin.listchars'):update() end }
