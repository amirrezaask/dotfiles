local listchars = function(chars)
  vim.opt.list = true
  vim.opt.listchars = chars 
end

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
