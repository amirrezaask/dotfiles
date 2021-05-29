vim.augroup({
  lua = {
    'BufEnter',
    '*.lua',
    function()
      vim.bo.ts = 2
      vim.bo.sw = 2
      vim.bo.sts = 2
      vim.bo.expandtab = true
    end,
  },
})
