vim.autocmd {
  "BufWritePost",
  "*",
  function()
    Actions:exec(0, "format")
    vim.c.edit()
  end,
}
