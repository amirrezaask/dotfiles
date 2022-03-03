local function make_listchars_str(tbl)
  local tbl_pairs = {}
  for name, value in pairs(tbl) do
    table.insert(tbl_pairs, string.format("%s:%s", name, value))
  end

  return table.concat(tbl_pairs, ",")
end
local str = make_listchars_str({
  eol = '↲',
  tab = '» ',
  trail = '·',
  extends= '<',
  precedes= '>',
  conceal= '┊',
  nbsp= '␣',
})

vim.opt.list = true
vim.opt.listchars = str
