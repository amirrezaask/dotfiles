vim.opt.guicursor = ""

vim.opt.nu = true
vim.opt.relativenumber = true

vim.opt.errorbells = false

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv "HOME" .. "/.vim/undodir"
vim.opt.undofile = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.termguicolors = true

vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.isfname:append "@-@"

-- Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
-- delays and poor user experience.
vim.opt.updatetime = 50

-- Don't pass messages to |ins-completion-menu|.
vim.opt.shortmess:append "c"

vim.opt.colorcolumn = "80"

vim.g.mapleader = " "

vim.opt.clipboard:append "unnamedplus" -- use system clipboard as default register.
vim.opt.colorcolumn = "120"

vim.opt.splitbelow = true
vim.opt.splitright = true

if vim.version().major >= 0 and vim.version().minor >= 8 then
  vim.opt.laststatus = 3 -- if supported use global statusline
end

-- Simple statusline
vim.opt.statusline = "%m%r%h%w%f %y"
