vim.keymap.set("n", "<C-enter>", function()
  require("terminal").toggle_right({ cmd = "go build ./..." })
end)

vim.keymap.set("n", "<M-enter>", function()
  require("terminal").get({ cmd = "go test ./..." })
end)
