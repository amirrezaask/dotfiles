vim.g.gruvbuddy_style = "dark"

if Has("rose-pine") then
  ---@diagnostic disable-next-line: missing-fields
  require("rose-pine").setup({
    styles = {
      italic = false,
      transparency = true,
    },
  })
end

if Has("gruvbox") then
  require("gruvbox").setup({
    contrast = "hard",
  })
end

function Transparent()
  vim.cmd [[
    hi Normal guibg=none
    hi NormalFloat guibg=none
    hi LineNr guibg=none
    hi SignColumn guibg=none
    hi FloatBorder guibg=none
  ]]
end

vim.api.nvim_create_user_command("Transparent", Transparent, {})

vim.cmd.colorscheme(Options.Colorscheme)
