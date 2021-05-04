-- Set all my global options
local nvim = require('amirrezaask.nvim')

nvim.with_options({
  ignorecase = true,
  modeline = true,
  autoread = true,
  compatible = false,
  encoding = 'utf-8',
  hlsearch = true,
  history = 700,
  tabpagemax = 100,
  ruler = true,
  mouse = 'a',
  wrap = true,
  autoindent = true,
  termguicolors = true,
  tabstop = 4,
  shiftwidth = 4,
  softtabstop = 4,
  expandtab = true,
  backup = false,
  writebackup = false,
  swapfile = false,
  clipboard = 'unnamedplus',
  splitright = true,
  splitbelow = true,
  cursorline = true,
  relativenumber = true,
  number = true,
  pumblend = 13,
  shortmess = "aI",
})

vim.g.netrw_banner = 0
