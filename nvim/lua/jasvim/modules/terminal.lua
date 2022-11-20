require("toggleterm").setup {
  size = function(term)
    if term.direction == "horizontal" then
      return 20
    elseif term.direction == "vertical" then
      return vim.o.columns * 0.4
    end
  end,
  direction = "horizontal",
  float_opts = {
    border = "single",
    height = math.ceil(window_height() * 0.5),
    width = math.ceil(window_width() * 0.3),
    winblend = 3,
  },
}