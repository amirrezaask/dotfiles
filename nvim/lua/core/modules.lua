local plugin_manager = require"core.plugin_manager"

plugin_manager.bootstrap()

local M = {}

setmetatable(M,
  {
    __call = function (_, names)
      for _, name in ipairs(names) do
        require ("modules." .. name .. ".plugins")
      end

      for _, name in ipairs(names) do
        require("modules." .. name .. ".options")
        require("modules." .. name .. ".commands")
        require("modules." .. name .. ".keymaps")
        require("modules." .. name .. ".autocmds")
        require("modules." .. name)
      end
    end
  }
)

return M
