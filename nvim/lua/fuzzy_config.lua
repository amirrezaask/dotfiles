local normal_maps = {}

local loc = require'fuzzy.lib.location'

require'fuzzy.lib.options'.setup {
  width = 70,
  height = 100,
  blacklist = {
    "vendor"
  },
  location = loc.bottom_center, 
  prompt = '> '
}
-- Commands
vim.cmd [[ command! IFiles lua require('fuzzy').interactive_finder{}  ]]
vim.cmd [[ command! Files lua require('fuzzy').find_files{} ]]
vim.cmd [[ command! Grep lua require('fuzzy').grep{} ]]
vim.cmd [[ command! Commands lua require('fuzzy').commands{} ]]
vim.cmd [[ command! MRU lua require('fuzzy').mru{} ]]
vim.cmd [[ command! BLines lua require('fuzzy').buffer_lines{} ]]
vim.cmd [[ command! Cd lua require('fuzzy').cd{} ]]
vim.cmd [[ command! Help lua require('fuzzy').help{} ]]
vim.cmd [[ command! Maps lua require('fuzzy').mappings{} ]]
vim.cmd [[ command! GitFiles lua require('fuzzy.git').git_files{} ]]
vim.cmd [[ command! GitGrep lua require('fuzzy.git').git_grep{} ]]
vim.cmd [[ command! GitCommits lua require('fuzzy.git').git_commits{} ]]
vim.cmd [[ command! GitBCommits lua require('fuzzy.git').git_bcommits{} ]]
vim.cmd [[ command! GitCheckout lua require('fuzzy.git').git_checkout{} ]]
vim.cmd [[ command! Buffers lua require('fuzzy').buffers{} ]]
vim.cmd [[ command! Rg lua require('fuzzy').rg{} ]]
vim.cmd [[ command! Colors lua require('fuzzy').colors{} ]]
vim.cmd [[ command! LspReferences lua require('fuzzy.lsp').lsp_references{} ]]
vim.cmd [[ command! LspDefinitions lua require('fuzzy.lsp').definitions{} ]]
vim.cmd [[ command! LspCodeActions lua require('fuzzy.lsp').code_actions{} ]]
vim.cmd [[ command! LspDocumentSymbols lua require('fuzzy.lsp').lsp_document_symbols{} ]]
vim.cmd [[ command! LspWorkspaceSymbols lua require('fuzzy.lsp').lsp_workspace_symbols{} ]]
-- Fuzzy.nvim
normal_maps['<Space><Space>'] = '<cmd>lua require("fuzzy").find_files{}<CR>'
normal_maps['<Space>fb'] = '<cmd>lua require("fuzzy").interactive_finder{}<CR>'
normal_maps['<Space>ec'] = '<cmd>lua require("fuzzy").find_files{path="/home/amirreza/src/github.com/amirrezaask/dotfiles"}<CR>'
normal_maps['<Space>en'] = '<cmd>lua require("fuzzy").find_files{path="/home/amirreza/.config/nvim"}<CR>'
normal_maps['<Space>ez'] = '<cmd>lua require("fuzzy").find_files{path="/home/amirreza/src/github.com/amirrezaask/dotfiles/zsh"}<CR>'
normal_maps['<Space>fp'] = '<cmd>lua require("fuzzy").find_files{path="/home/amirreza/.local/share/nvim/site/pack/packer/start"}<CR>'
normal_maps['<Space>gf'] = '<cmd>lua require("fuzzy.git").git_files{}<CR>'
normal_maps['<C-p>'] = '<cmd>lua require("fuzzy.git").git_files{}<CR>'
normal_maps['<Space>fr'] = '<cmd>lua require"fuzzy".mru{}<CR>'
normal_maps['<Space>pf'] = '<cmd>lua require("fuzzy").projects{locations={"/home/amirreza/src"}}<CR>'
normal_maps['??'] = '<cmd>lua require("fuzzy").grep{}<CR>'
normal_maps['<Space>b'] = '<cmd>lua require("fuzzy").buffers{}<CR>'
normal_maps['<Space>gg'] = '<cmd>lua require("fuzzy.git").git_grep{}<CR>'
normal_maps['<Space>c'] = '<cmd>lua require("fuzzy").commands{}<CR>'
normal_maps['<Space>h'] = '<cmd>lua require("fuzzy").history{}<CR>'
normal_maps['<Space>h'] = '<cmd>lua require("fuzzy").help{}<CR>'
normal_maps['<Space>gc'] = '<cmd>lua require("fuzzy.git").git_commits{}<CR>'
normal_maps['<Space>gb'] = '<cmd>lua require("fuzzy.git").git_bcommits{}<CR>'
normal_maps['<Space>gco'] = '<cmd>lua require("fuzzy.git").git_checkout{}<CR>'

require'nvim'.mode_map({
  n = normal_maps
})
