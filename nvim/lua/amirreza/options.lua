vim.opt.errorbells = false
vim.opt.smartindent = true
vim.opt.wrap = true
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = true
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.termguicolors = true
vim.opt.signcolumn = "yes"
vim.opt.isfname:append("@-@")
vim.opt.updatetime = 50
vim.opt.shortmess:append("c") -- Don't pass messages to |ins-completion-menu|.
vim.opt.shortmess:append("I") -- No Intro message
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.cursorline = false
vim.opt.sw = 4
vim.opt.ts = 4
vim.opt.expandtab = true
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.opt.timeoutlen = 300
vim.opt.completeopt = "menu"
vim.opt.statusline = "%q%w%h%r%m%f %y %l:%c %p%%"
vim.opt.ignorecase = true
vim.opt.title = true
vim.opt.titlestring = '%F'
vim.opt.breakindent = true
vim.opt.number = true
vim.opt.relativenumber = true

IS_WINDOWS = vim.fn.has("win32") == 1
