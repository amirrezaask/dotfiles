NLINE_STATUS_GENERATOR = nil

local function process(e)
  if type(e) == "function" then
    return process(e())
  elseif type(e) == 'string' then
    return e
  elseif type(e) == 'table' then
    return table.concat(e, '')
  end
end

local function make_statusline(elements, opts)
  opts = opts or {}
  NLINE_STATUS_GENERATOR = function()
    local _parts = {}
    for _, e in ipairs(elements) do
      local processed = process(e)
      table.insert(_parts, processed)
    end
    return table.concat(_parts, opts.delimiter or '')
  end
end

vim.opt.statusline = '%!v:lua.NLINE_STATUS_GENERATOR()'

return {
  make = make_statusline,
  parts = require('nline.parts'),
  wrappers = require('nline.wrappers'),
}

