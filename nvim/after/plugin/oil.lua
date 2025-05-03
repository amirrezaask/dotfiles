if Missing("oil") then
  return
end

require("oil").setup {}

vim.keymap.set("n", "-", "<cmd>Oil<CR>")
