local M = {}

M.list_chars = {
  eol = '↲',
  tab = '» ',
  trail = '·',
  extends = '<',
  precedes = '>',
  conceal = '┊',
  nbsp = '␣',
}

function M:update(chars)
  chars = chars or self.list_chars
  vim.cmd([[ set list ]])
  local list_chars_tuple = {}
  for k, v in pairs(chars) do
    if v ~= '' then
      table.insert(list_chars_tuple, string.format('%s:%s', k, v))
    end
  end
  vim.cmd(string.format('let &listchars="%s"', table.concat(list_chars_tuple, ',')))
end

M:update()
return setmetatable(M, {
  __newindex = function(t, k, v)
    t.list_chars[k] = v
    t:update()
  end,
  __call = function(t, _) t:update() end
})
