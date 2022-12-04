local ok, _ = pcall(require, "telescope")
if not ok then
  return
end

local keymaps = require "amirrezaask.keymaps"
local nnoremap = keymaps.nnoremap

require("telescope").setup {}

nnoremap("<leader><leader>", function()
  require("telescope.builtin").find_files()
end)

nnoremap("<leader>ff", function()
  require("telescope.builtin").find_files()
end)

nnoremap("<C-P>", function()
  require("telescope.builtin").commands(require("telescope.themes").get_dropdown())
end)

nnoremap("<leader>fw", function()
  require("telescope.builtin").live_grep()
end)

nnoremap("??", function()
  require("telescope.builtin").live_grep()
end)
