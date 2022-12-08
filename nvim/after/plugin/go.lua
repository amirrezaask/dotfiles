local keymaps = require "core.keymaps"

vim.g.go_gopls_enabled = 0
vim.g.go_template_autocreate = 0

local go_group = vim.api.nvim_create_augroup("go", {})

vim.api.nvim_create_autocmd("BufEnter", {
  pattern = "*.go",
  group = go_group,
  callback = function(meta)
    local buffer = { buffer = meta.bufnr, remap = true }
    keymaps.nnoremap("<leader>lat", "<cmd>GoAddTag<CR>", buffer)
    keymaps.nnoremap("<leader>lrt", "<cmd>GoRmTag<CR>", buffer)
    keymaps.nnoremap("<leader>lfs", "<cmd>GoFillStruct<CR>", buffer)
  end,
})
