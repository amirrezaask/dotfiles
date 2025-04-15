require("gitsigns").setup {}

vim.keymap.set("n", "B", ":Gitsigns blame<CR>")
-- Add more keys for reviewing hunks
