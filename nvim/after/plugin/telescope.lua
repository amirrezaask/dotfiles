local ok, _ = pcall(require, "telescope")
if not ok then
  return
end

local keymaps = require "amirrezaask.keymaps"
local nnoremap = keymaps.nnoremap

require("telescope").setup {}
require("telescope").load_extension "fzf"

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

nnoremap("<leader>fc", function()
  require("telescope.builtin").commands()
end)

nnoremap("<leader>fh", function()
  require("telescope.builtin").help_tags()
end)

nnoremap("<leader>fk", function()
  require("telescope.builtin").keymaps()
end)

-- Edit configurations
nnoremap("<leader>en", function()
  require("telescope.builtin").find_files { cwd = "~/.config/nvim" }
end)

nnoremap("<leader>ek", function()
  require("telescope.builtin").find_files { cwd = "~/.config/kitty" }
end)
