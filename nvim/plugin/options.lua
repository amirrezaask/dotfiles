vim.o.wrap = true
vim.o.breakindent = true
vim.o.signcolumn = "yes"
vim.o.swapfile = false
vim.o.backup = false
vim.o.undofile = true
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.showmode = false
vim.o.shiftwidth = 4
vim.o.tabstop = 4
vim.o.expandtab = true
vim.o.timeoutlen = 300
vim.o.updatetime = 250
vim.o.clipboard = "unnamedplus"
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.cursorline = true -- Highlight current line
vim.o.guicursor = vim.o.guicursor .. ",t:ver25"
vim.o.laststatus = 3    -- Single Statusline for all windows
vim.o.number = true     -- Line numbers
vim.o.winblend = 10     -- Floating Windows Transparency
vim.o.termguicolors = true
vim.o.winborder = 'rounded'
vim.o.inccommand = 'split'
vim.o.more = false
require("statusline") -- Loads lua/statusline/init.lua which is a simple script to create a beautiful statusline
