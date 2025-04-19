keymap("n", "<C-enter>", function() require("terminal").get({ cmd = "go build ./..." }) end)
keymap("n", "<M-enter>", function() require("terminal").get({ cmd = "go test ./..." }) end)
