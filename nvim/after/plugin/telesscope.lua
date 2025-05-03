if Missing("telescope") then
  return
end

require("telescope").setup {}

require("telescope").load_extension("ui-select")
require("telescope").load_extension("fzf")

local builtin = require("telescope.builtin")

vim.keymap.set("n", "<leader><leader>", builtin.find_files, { desc = "Find files" })
vim.keymap.set("n", "??", builtin.live_grep, { desc = "Find in files" })
vim.keymap.set("n", "<leader>h", builtin.help_tags, { desc = "Find help" })
vim.keymap.set("n", "<leader>fd", function()
  builtin.find_files { cwd = vim.fn.stdpath "config" }
end, { desc = "Find files in config" })
vim.keymap.set("v", "??", builtin.grep_string, { desc = "Find word under cursor" })
vim.keymap.set("n", "<leader>fs", function()
  vim.ui.input({ prompt = "Grep word: " }, function(input)
    if input == "" or input == nil then
      return
    end
    builtin.grep_string { search = input }
  end)
end, { desc = "Grep" })
