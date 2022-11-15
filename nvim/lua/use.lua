local M = {}

local function build_spec(spec)

end

function M.use(spec)
  table.insert(M.specs, spec)
end

function M.build()
  for _, spec in ipairs(M.specs) do
    build_spec(spec)
  end
end

setmetatable(M, {
  __call = function(tbl, spec)
    tbl:use(spec)
  end,
})

return M
