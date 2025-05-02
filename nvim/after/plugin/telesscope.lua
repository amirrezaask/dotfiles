if Missing("telescope") then
  return
end

local builtin = require("telescope.builtin")

vim.keymap.set("n", "<leader><leader>", builtin.find_files, { desc = "Find files" })
vim.keymap.set("n", "??", builtin.live_grep, { desc = "Find in files" })
vim.keymap.set("n", "<leader>h", builtin.help_tags, { desc = "Find help" })
vim.keymap.set("v", "??", builtin.grep_string, { desc = "Find word under cursor" })
vim.keymap.set("n", "<leader>fs", function()
  vim.ui.input({ prompt = "Grep word: " }, function(input)
    if input == "" or input == nil then
      return
    end
    builtin.grep_string { search = input }
  end)
end, { desc = "Grep" })
