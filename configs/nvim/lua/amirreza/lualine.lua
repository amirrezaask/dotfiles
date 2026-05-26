vim.pack.add {
  "https://github.com/nvim-lualine/lualine.nvim",
}

require("lualine").setup {
  sections = {
    lualine_c = {
      { "filename", path = 1 },
    },
    lualine_x = {
      "filetype",
    },
  },
}
