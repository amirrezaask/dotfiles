_G.IDE = {}
_G.Commands = {}

vim.keymap.set("n", "<leader>ff", "<cmd>lua _G.IDE.Files()<CR>")
vim.keymap.set("n", "<leader><leader>", "<cmd>lua _G.IDE.Files()<CR>")
vim.keymap.set("n", "<leader>b", "<cmd>lua _G.IDE.Buffers()<CR>")
vim.keymap.set("n", "<leader>h", "<cmd>lua _G.IDE.Help()<CR>")
vim.keymap.set("n", "<C-p>", "<cmd>lua _G.IDE.GitFiles()<CR>")
vim.keymap.set("n", "??", "<cmd>lua _G.IDE.Grep()<CR>")
vim.keymap.set("n", "<leader>o", "<cmd>lua _G.IDE.DocumentSymbols()<CR>")
vim.keymap.set("n", "<leader>O", "<cmd>lua _G.IDE.WorkspaceSymbols()<CR>")
vim.keymap.set("n", "<leader>;", "<cmd>lua _G.IDE.Commands()<CR>")

-- Commands
vim.cmd([[ command! Commits lua _G.IDE.GitCommits() ]])
vim.cmd([[ command! Branches lua _G.IDE.GitBranches() ]])
vim.cmd([[ command! DocumentSymbols lua _G.IDE.DocumentSymbols() ]])
vim.cmd([[ command! WorkspaceSymbols lua _G.IDE.WorkspaceSymbols() ]])

table.insert(_G.Commands, "Commits")
table.insert(_G.Commands, "Branches")
table.insert(_G.Commands, "DocumentSymbols")
table.insert(_G.Commands, "WorkspaceSymbols")
