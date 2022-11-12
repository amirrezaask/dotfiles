local plugin_manager = require"core.plugin_manager"

-- Init the packer
plugin_manager.bootstrap()

local M = {}

setmetatable(M,
  {
    __call = function (_, names)
      local to_load = {}
      table.insert(to_load, "core.keymaps")
      table.insert(to_load, "core.ui")
      table.insert(to_load, "core.lsp")
      table.insert(to_load, "core.treesitter")

      for _, name in ipairs(names) do
        table.insert(to_load, "modules." .. name)
      end

      for _, name in ipairs(to_load) do
        pcall(require, (name .. ".plugins"))
      end

      plugin_manager.install()

      for _, name in ipairs(to_load) do
        pcall(require, (name .. ".options"))
        pcall(require, (name .. ".commands"))
        pcall(require, (name .. ".keymaps"))
        pcall(require, (name .. ".autocmds"))
        pcall(require, (name))
      end
    end
  }
)

return M
