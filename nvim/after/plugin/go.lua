local keymaps = require "core.keymaps"

vim.g.go_gopls_enabled = 0
vim.g.go_template_autocreate = 0

local go_group = vim.api.nvim_create_augroup("go", {})

vim.api.nvim_create_autocmd("BufEnter", {
  pattern = "*.go",
  group = go_group,
  callback = function(meta)
    keymaps.buf_nnoremap(meta.buffer, "<leader>lat", "<cmd>GoAddTag<CR>", { remap = true })
    keymaps.buf_nnoremap(meta.buffer, "<leader>lrt", "<cmd>GoRmTag<CR>", { remap = true })
    keymaps.buf_nnoremap(meta.buffer, "<leader>lfs", "<cmd>GoFillStruct<CR>", { remap = true })
  end,
})
