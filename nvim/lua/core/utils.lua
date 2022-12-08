local function has_plugins(plugins)
  if type(plugins) == "table" then
    for _, name in ipairs(plugins) do
      local ok, _ = pcall(require, name)
      if not ok then
        return false
      end
    end
  elseif type(plugins) == "string" then
    local ok, _ = pcall(require, plugins)
    if not ok then
      return false
    end
  end

  return true
end

_G.has_plugins = has_plugins
