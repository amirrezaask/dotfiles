local plugin_manager = require"core.plugin_manager"
plugin_manager.bootstrap()

local M = {}

function M.load(name)
  require("modules." .. name .. ".commands")
  require("modules." .. name .. ".keymaps")
  require("modules." .. name .. ".autocmds")
end


function M.load_modules(names)
  for _, name in ipairs(names) do
    require ("modules." .. name .. ".plugins")
  end

  for _, name in ipairs(names) do
    M.load(name)
  end
end


return M
