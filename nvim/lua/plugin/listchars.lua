vim.opt.list = true

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
    vim.opt.listchar = yaml_listchars
  end
}

vim.autocmd {
  "BufLeave",
  "*.yaml,*.yml",
  function()
    vim.opt.listchar = default_listchars
  end
}

vim.opt.listchars = default_listchars
