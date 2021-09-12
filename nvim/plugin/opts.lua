vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.equalalways = false
vim.opt.modeline = true
vim.opt.autoread = true
vim.opt.compatible = false
vim.opt.encoding = "utf-8"
vim.opt.hlsearch = true
vim.opt.history = 700
vim.opt.tabpagemax = 100
vim.opt.ruler = true
vim.opt.mouse = "a"
vim.opt.wrap = true
vim.opt.autoindent = true
vim.opt.termguicolors = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.cursorline = true
vim.opt.relativenumber = true
vim.opt.number = true
vim.opt.pumblend = 5
vim.opt.showmode = false
vim.opt.clipboard = "unnamedplus"
vim.opt.hidden = true
vim.opt.updatetime = 100
vim.opt.wildmode = { "longest", "list", "full" }
vim.opt.wildmode = vim.opt.wildmode - "list"
vim.opt.wildmode = vim.opt.wildmode + { "longest", "full" }
vim.opt.colorcolumn = "80"

if vim.fn.executable "rg" == 1 then
  vim.opt.grepprg = "rg --vimgrep --no-heading"
  vim.opt.grepformat = "%f:%l:%c:%m,%f:%l:%m"
end
