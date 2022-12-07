local M = {}

function M.has_plugins(plugins)
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

function M.get_command(name)
  local all = vim.api.nvim_get_commands {}
  for _, cmd in pairs(all) do
    if cmd.name == name then
      return cmd
    end
  end
end

return M
