if vim.g.nvim_picker == "fzf" then
  require("amirreza.picker.fzf")
else
  require("amirreza.picker.snacks")
end
