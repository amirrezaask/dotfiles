require("telescope").load_extension "harpoon"
local nnoremap = require("amirrezaask.keymaps").nnoremap

nnoremap("<C-e>", require("harpoon.ui").toggle_quick_menu)

nnoremap("<leader>a", require("harpoon.mark").add_file)

nnoremap("<C-1>", function()
  require("harpoon.ui").nav_file(1)
end)

nnoremap("<C-2>", function()
  require("harpoon.ui").nav_file(2)
end)

nnoremap("<C-3>", function()
  require("harpoon.ui").nav_file(3)
end)

nnoremap("<C-4>", function()
  require("harpoon.ui").nav_file(4)
end)

nnoremap("<C-5>", function()
  require("harpoon.ui").nav_file(5)
end)

nnoremap("<C-6>", function()
  require("harpoon.ui").nav_file(6)
end)

-- I dont use these two keys so I remap them for harpoon
nnoremap("L", require("harpoon.ui").nav_next)
nnoremap("H", require("harpoon.ui").nav_prev)
