vim.keymap.set("n", "<C-enter>", function()
  require("terminal").get({ cmd = "go build ./..." })
end)

vim.keymap.set("n", "<M-enter>", function()
  require("terminal").get({ cmd = "go test ./..." })
end)
