vim.pack.add {
  "https://github.com/folke/snacks.nvim",
}

Snacks = require("snacks")

require("snacks").setup {
  bigfile = { enabled = true },
  indent = { enabled = true },
  input = { enabled = true },
  picker = { enabled = true },
  notifier = { enabled = true },
  quickfile = { enabled = true },
  statuscolumn = { enabled = true },
}

vim.keymap.set("n", "<leader><leader>", Snacks.picker.files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>i", function() Snacks.picker.files { cwd = "~/dev/dotfiles" } end, { desc = "Find Configuration" })
vim.keymap.set("n", "<leader>pf", Snacks.picker.git_files, { desc = "Git Files" })
vim.keymap.set("n", "<leader>gl", Snacks.picker.git_log, { desc = "Git Log" })
vim.keymap.set("n", "<leader>gL", Snacks.picker.git_log_line, { desc = "Git Log Line" })
vim.keymap.set("n", "<leader>j", Snacks.picker.grep, { desc = "Grep" })
vim.keymap.set("n", "<leader>k", Snacks.picker.buffers, { desc = "Buffers" })
vim.keymap.set("n", "<leader>;", Snacks.picker.commands, { desc = "Commands" })
vim.keymap.set("n", "<leader>c", Snacks.picker.colorschemes, { desc = "Commands" })
vim.keymap.set({ "n", "v" }, "<leader>J", Snacks.picker.grep_word, { desc = "Grep Word" })
vim.keymap.set("n", "<leader>e", Snacks.explorer.reveal, { desc = "Reveal current file/buffer file list" })

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    vim.keymap.set("n", "gd", Snacks.picker.lsp_definitions, { buffer = args.buf, desc = "[g]oto [d]efinition" })
    vim.keymap.set("n", "grr", Snacks.picker.lsp_references, { buffer = args.buf, desc = "[g]oto [r]eferences" })
    vim.keymap.set("n", "gri", Snacks.picker.lsp_implementations, { buffer = args.buf, desc = "[g]oto [i]mplmentations" })
    vim.keymap.set("n", "gO", Snacks.picker.lsp_symbols, { buffer = args.buf, desc = "[g]oto symbol" })
    vim.keymap.set("n", "<leader>o", Snacks.picker.lsp_symbols, { buffer = args.buf, desc = "[s]ymbols (outline)" })
    vim.keymap.set("n", "<leader>O", Snacks.picker.lsp_workspace_symbols, { buffer = args.buf, desc = "[s]ymbols [w]orkspace" })
    vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf, desc = "Code Actions" })
  end,
})
