local M = {}

__Current_List_Chars = {}

function M:update(chars)
  vim.opt.list = true
  local list_chars_tuple = {}
  for k, v in pairs(chars) do
    if v ~= '' then
      table.insert(list_chars_tuple, string.format('%s:%s', k, v))
    end
  end
  vim.opt.listchars = list_chars_tuple
end

return setmetatable(M, {
  __newindex = function(t, k, v)
    __Current_List_Chars[k] = v
    t:update(__Current_List_Chars)
  end,
  __call = function(t, chars) 
    __Current_List_Chars = chars
    t:update(chars)
  end
})
