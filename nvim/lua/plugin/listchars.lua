local listchars = require('listchars')
local default_listchars = {
  eol = '↲',
  tab = '»\\ ',
  trail = '·',
  extends = '<',
  precedes = '>',
  conceal = '┊',
  nbsp = '␣',
}

local yaml_listchars = {
  lead ='·',
  trail='*',
  eol = '↲',
  tab = '->\\ ',
  extends = '<',
  precedes = '>',
  conceal = '┊',
  nbsp = '␣',
}

vim.autocmd {
  "BufEnter",
  "*.yaml,*.yml",
  function()
    print("yaml mode")
    listchars(yaml_listchars)
  end
}

vim.autocmd {
  "BufLeave",
  "*.yaml,*.yml",
  function()
    listchars(default_listchars)
  end
}

listchars(default_listchars)
