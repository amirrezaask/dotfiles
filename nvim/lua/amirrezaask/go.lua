require("go").setup()
vim.cmd [[ command! GoCommands lua GO_telescope_picker() ]]

vim.cmd [[ augroup Go ]]
vim.cmd [[ autocmd BufWritePre *.go :silent! lua require('go.format').gofmt() ]]
vim.cmd [[ autocmd BufWritePre *.go :silent! lua require('go.format').goimport() ]]
vim.cmd [[ augroup END ]]
