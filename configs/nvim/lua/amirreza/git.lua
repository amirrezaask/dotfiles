vim.pack.add {
  "https://github.com/tpope/vim-fugitive",
  "https://github.com/lewis6991/gitsigns.nvim",
  "https://github.com/sindrets/diffview.nvim",
}

-- Diffview
require("diffview").setup {}
vim.keymap.set("n", "<leader>G", "<cmd>DiffviewOpen<CR>")
vim.keymap.set("n", "<leader>gr", "<cmd>DiffviewRefresh<CR>", { desc = "Diffview Refresh" })

-- Gitsigns
local gitsigns = require("gitsigns")
gitsigns.setup {
  current_line_blame = true,
  on_attach = function(bufnr)
    vim.keymap.set("n", "<leader>gb", gitsigns.blame_line, { buffer = bufnr, desc = "Blame Line" })
    vim.keymap.set("n", "<leader>gp", gitsigns.preview_hunk, { buffer = bufnr, desc = "Preview Hunk" })
    vim.keymap.set("n", "<leader>gn", gitsigns.next_hunk, { buffer = bufnr, desc = "Next Hunk" })
    vim.keymap.set("n", "<leader>gN", gitsigns.prev_hunk, { buffer = bufnr, desc = "Prev Hunk" })
  end,
}
