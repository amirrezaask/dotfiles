vim.pack.add {
  "https://github.com/vague-theme/vague.nvim",
  "https://github.com/sainnhe/everforest",
  "https://github.com/morhetz/gruvbox",
  { src = "https://github.com/catppuccin/nvim", name = "catppuccin" },
  { src = "https://github.com/rose-pine/neovim", name = "rose-pine" },
  "https://github.com/scottmckendry/cyberdream.nvim",
  "https://github.com/navarasu/onedark.nvim",
  "https://github.com/folke/tokyonight.nvim",
}

require("vague").setup { bold = false, italic = false }
require("onedark").setup { style = "darker" }
require("tokyonight").setup {
  styles = { comments = { italic = false }, keywords = { italic = false } },
}
require("catppuccin").setup { transparent_background = true }
require("rose-pine").setup {
  styles = { bold = false, italic = false, transparency = true },
}

vim.g.everforest_background = "hard"
vim.g.everforest_enable_italic = 0
vim.g.gruvbox_contrast_dark = "hard"

vim.api.nvim_create_autocmd("ColorScheme", {
  pattern = "everforest",
  callback = function()
    if vim.o.background == "dark" then
      vim.cmd([[
        hi! Normal guibg=#1e2326 guifg=#ffffff
        hi! NormalFloat guibg=#1e2326 guifg=#ffffff
        hi! Terminal guibg=#1e2326 guifg=#ffffff
      ]])
    end
  end,
})

vim.api.nvim_create_autocmd("ColorScheme", {
  pattern = "gruvbox",
  callback = function()
    if vim.o.background == "dark" then
      vim.cmd([[
        hi! Normal guibg=none guifg=#ffffff
        hi! NormalFloat guibg=none guifg=#ffffff
        hi! Terminal guibg=none guifg=#ffffff
      ]])
    end
  end,
})

vim.cmd.colorscheme(vim.g.nvim_colorscheme)
