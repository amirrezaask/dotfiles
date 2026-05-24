vim.pack.add {
  "https://github.com/nvim-treesitter/nvim-treesitter",
  "https://github.com/nvim-treesitter/nvim-treesitter-context",
}

vim.api.nvim_create_autocmd("FileType", {
  callback = function(args) pcall(vim.treesitter.start, args.buf) end,
})

require("treesitter-context").setup {
  enable = true,
}

require("nvim-treesitter").install {
  "bash",
  "c",
  "cpp",
  "fish",
  "gitcommit",
  "go",
  "graphql",
  "html",
  "hyprlang",
  "java",
  "javascript",
  "json",
  "json5",
  "lua",
  "markdown",
  "markdown_inline",
  "python",
  "query",
  "rasi",
  "regex",
  "rust",
  "scss",
  "toml",
  "tsx",
  "typescript",
  "vim",
  "vimdoc",
  "yaml",
}
