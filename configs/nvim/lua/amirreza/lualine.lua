vim.pack.add {
  "https://github.com/nvim-lualine/lualine.nvim",
}

require("lualine").setup {
  options = {
    theme = "catppuccin-macchiato",
    globalstatus = true,
    component_separators = { left = "", right = "" },
    section_separators = { left = "█", right = "█" },
  },
  sections = {
    lualine_c = {
      { "filename", path = 1 },
    },
    lualine_x = {
      "filetype",
    },
  },
}
