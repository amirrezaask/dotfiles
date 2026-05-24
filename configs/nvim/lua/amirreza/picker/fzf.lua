vim.pack.add {
  "https://github.com/ibhagwan/fzf-lua",
}

require("fzf-lua").setup { "telescope" }
FzfLua = require("fzf-lua")

vim.keymap.set("n", "<leader><leader>", FzfLua.files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>i", function() FzfLua.files { cwd = "~/dev/dotfiles" } end, { desc = "Find Configuration" })
vim.keymap.set("n", "<leader>pf", FzfLua.git_files, { desc = "Git Files" })
vim.keymap.set("n", "<leader>k", FzfLua.buffers, { desc = "Buffers" })
vim.keymap.set("n", "<leader>j", FzfLua.live_grep, { desc = "Grep" })
vim.keymap.set("n", "<leader>h", FzfLua.helptags, { desc = "Help Tags" })
vim.keymap.set("n", "<leader>l", FzfLua.diagnostics_document, { desc = "Diagnostics Document" })
vim.keymap.set("n", "<leader>L", FzfLua.diagnostics_workspace, { desc = "Diagnostics Workspace" })
vim.keymap.set("n", "<leader>;", FzfLua.commands, { desc = "Commands" })
vim.keymap.set("n", "<leader>n", function() FzfLua.files { cwd = "~/dev/notes" } end, { desc = "Notes" })
vim.keymap.set({ "n", "v", "x" }, "<leader>J", FzfLua.grep_cword, { desc = "Grep Word" })
FzfLua.register_ui_select()

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local opts = { buffer = args.buf }
    vim.keymap.set("n", "gd", FzfLua.lsp_definitions, opts)
    vim.keymap.set("n", "grr", FzfLua.lsp_references, opts)
    vim.keymap.set("n", "gri", FzfLua.lsp_implementations, opts)
    vim.keymap.set("n", "C", FzfLua.lsp_code_actions, opts)
    vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf, desc = "Open Floating Diagnostic" })
  end,
})
