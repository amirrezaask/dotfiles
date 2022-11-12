local plugin_manager = require"core.plugin_manager"

-- Init the packer
plugin_manager.bootstrap()

require "core.lsp"
require "core.treesitter"
require "core.netrw"
require "core.ui"

local M = {}

setmetatable(M,
  {
    __call = function (_, names)
      for _, name in ipairs(names) do
        require ("modules." .. name .. ".plugins")
      end

      plugin_manager.install()
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
