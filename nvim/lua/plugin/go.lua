vim.g.go_imports_on_save = true
vim.autocmd {
  "BufWritePost",
  "*",
  function()
    if vim.fn.executable("goimports") == 1 and vim.g.go_imports_on_save then
      local filename = vim.api.nvim_buf_get_name(0)
      vim.fn.system(string.format("goimports -w %s", filename))
      vim.c.edit()
    end
  end
}
