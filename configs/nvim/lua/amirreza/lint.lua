vim.pack.add {
  "https://github.com/mfussenegger/nvim-lint",
}

require("lint").linters_by_ft = {
  typescript = { "eslint_d" },
  typescriptreact = { "eslint_d" },
  go = { "golangcilint" },
}

vim.api.nvim_create_autocmd({ "BufWritePost", "BufReadPost", "BufEnter", "FocusGained" }, {
  callback = function() pcall(require("lint").try_lint) end,
})
