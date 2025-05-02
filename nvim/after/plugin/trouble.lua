if Missing("trouble") then
  return
end

local trouble = require("trouble")
---@diagnostic disable-next-line: missing-fields
trouble.setup {}

vim.keymap.set("n", "<C-e>", "<cmd>Trouble diagnostics toggle<CR>", { noremap = true })
