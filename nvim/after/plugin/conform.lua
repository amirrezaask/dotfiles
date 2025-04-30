if Missing("conform") then
  return
end

require("conform").setup({
  formatters_by_ft = {
    lua = { "stylua" },
    go = { "goimports" },
    ocmal = { "ocamlformat" },
    php = {},
  },
})

vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = { "*.lua", "*.go" },
  callback = function(args)
    require("conform").format({ bufnr = args.buf })
  end,
})
