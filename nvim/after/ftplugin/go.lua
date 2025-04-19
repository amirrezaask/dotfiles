keymap("n", "<C-enter>", function() require("terminal").floating_with_command("go build ./...") end)
keymap("n", "<M-enter>", function() require("terminal").floating_with_command("go test ./...") end)
