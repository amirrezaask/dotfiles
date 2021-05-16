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
  splitright = true,
  splitbelow = true,
  cursorline = true,
  relativenumber = true,
  number = true,
  pumblend = 13,
  colorcolumn = '120',
})

vim.cmd [[ set clipboard=unnamedplus ]]

vim.g.netrw_banner = 0
vim.g.go_fmt_command = 'goimports'
vim.g.go_test_show_name = 1
vim.g.go_list_type = "quickfix"
vim.g.go_autodetect_gopath = 1
-- vim.g.go_metalinter_autosave_enabled = ['vet', 'golint']
-- vim.g.go_metalinter_enabled = ['vet', 'golint']
vim.g.go_gopls_complete_unimported = 1
vim.g.go_diagnostics_level = 2 
vim.g.go_doc_popup_window = 1
vim.g.go_imports_mode="gopls"
vim.g.go_imports_autosave=1
vim.g.go_highlight_build_constraints = 1
vim.g.go_highlight_operators = 1
