local M = {}

vim.g.mapleader = ' '

function M.bind(spec)
  for mode, keys in pairs(spec) do
    for key, binding in pairs(keys) do
      vim.keymap.set(mode, key, binding)
    end
  end
end

-- M.bind {
--   n = {
--     ['Q'] = '<NOP>',
--   }
-- }


return M
