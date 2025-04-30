if Missing("snacks") then
  return
end

require("snacks").setup {
  picker = { enabled = true },
}

Snacks = require("snacks")
SnacksPicker = Snacks.picker

vim.keymap.set("n", "<leader><leader>", SnacksPicker.files, { desc = "Find Files" })
vim.keymap.set("n", "<C-r>", SnacksPicker.files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>b", SnacksPicker.buffers, { desc = "Find Buffers" })
vim.keymap.set("n", "<leader>h", SnacksPicker.help, { desc = "Vim Help Tags" })
vim.keymap.set("n", "<C-p>", SnacksPicker.git_files, { desc = "Git Files" })
vim.keymap.set("n", "??", SnacksPicker.grep, { desc = "Live Grep" })
vim.keymap.set("n", "<leader>fw", function()
  vim.ui.input({ prompt = "Grep word: " }, function(input)
    if input == "" or input == nil then
      return
    end
    SnacksPicker.grep_word({
      search = function(_)
        return input
      end,
    })
  end)
end, { desc = "Grep" })
vim.keymap.set("v", "??", SnacksPicker.grep_word, { desc = "Grep word under cursor" })
vim.keymap.set("n", "<leader>o", SnacksPicker.lsp_symbols, { desc = "LSP Document Symbols" })
vim.keymap.set("n", "<leader>O", SnacksPicker.lsp_workspace_symbols, { desc = "LSP Workspace Symbols" })
vim.keymap.set("n", "<M-o>", SnacksPicker.lsp_workspace_symbols, { desc = "LSP Workspace Symbols" })
vim.keymap.set("n", "<leader>fd", function()
  SnacksPicker.files({ cwd = "~/.dotfiles" })
end, { desc = "Find Dotfiles" })

vim.lsp.buf.definition = SnacksPicker.lsp_definitions
vim.lsp.buf.implementation = SnacksPicker.lsp_implementations
vim.lsp.buf.references = SnacksPicker.lsp_references
vim.lsp.buf.type_definition = SnacksPicker.lsp_type_definitions
