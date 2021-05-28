local listchars = function(chars)
  vim.opt.list = true
  local list_chars_tuple = {}
  for k, v in pairs(chars) do
    if v ~= '' then
      table.insert(list_chars_tuple, string.format('%s:%s', k, v))
    end
  end
  vim.opt.listchars = list_chars_tuple
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
