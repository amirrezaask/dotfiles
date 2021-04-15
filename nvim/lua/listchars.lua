local M = {}

M.list_chars = {
  eol = '↲',
  tab = '» ',
  space = '',
  lead = '',
  trail = '·',
  extends = '<',
  precedes = '>',
  conceal = '┊',
  nbsp = '␣',
}

function M.update(self, chars)
  chars = chars or self.list_chars
  vim.cmd [[ set list ]]
  local list_chars_tuple = {}
  for k,v in pairs(chars) do
    if v == '' then goto continue end
    table.insert(list_chars_tuple, string.format('%s:%s', k,v))
    ::continue::
  end
  vim.cmd(string.format('let &listchars="%s"', table.concat(list_chars_tuple, ',')))

end 

return setmetatable(M, {
  __newindex = function(t, k,v)
    t.list_chars[k] = v
  end
})
