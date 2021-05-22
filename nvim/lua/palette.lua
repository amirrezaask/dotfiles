local palette = {}

local highlight = {}
highlight.__index = highlight

-- {
--   name = 'Normal',
--   name = {'TSComment', 'Comment'}
--   either
--   bg = '',
--   fg = '',
--   styles = {}
--   or
--   link = ''
-- }
function highlight:_apply_link()
  return table.concat({'highlight', 'link', self.name, self.link}, ' ')
end
function highlight:_apply_actual()
  if type(self.name) == 'table' and vim.tbl_islist(self.name) then
    local hls = {}
    for _, name in ipairs(self.name) do
      local this_hl = highlight:new {
        name = name,
        bg = self.bg,
        fg = self.fg,
        styles = self.styles
      }
      this_hl:apply()
      table.insert(hls, this_hl)
    end
  elseif type(self.name) == 'string' then
    local output = {'highlight', self.name}
    if self.bg then
      table.insert(output, string.format('guibg=%s', self.bg))
    end
    if self.fg then
      table.insert(output, string.format('guifg=%s', self.fg))
    end
    if self.styles then
      table.insert(output, string.format('gui=%s', table.concat(self.styles, ',')))
    end
    vim.cmd(table.concat(output, ' '))
  else
    vim.api.nvim_err_writeln('not supported type for name: ' .. type(self.name))
  end
end
function highlight:apply()
  if self.link then
    self:_apply_link()
  else
    self:_apply_actual()
  end
end

function highlight:new(opts)
  assert(opts, 'need opts')
  assert(opts.name, 'need a name')
  assert((type(opts.name) == 'string' or (type(opts.name)=='table' and vim.tbl_islist(opts.name))),
    'name should be either a table of strings or a simple string')
  return setmetatable(opts, self)
end

return palette
