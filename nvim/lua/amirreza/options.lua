vim.o.wrap = true -- Wrap long lines.
vim.o.breakindent = true -- Indent wrapped lines.
vim.o.signcolumn = "yes" -- Show signcolumn.
vim.o.swapfile = false -- Disable swapfile.
vim.o.undofile = true -- Store undo history on disk
vim.o.splitbelow = true -- Split windows below the current windows
vim.o.splitright = true -- Split windows right to the current windows
vim.o.showmode = false -- Don't show Vim mode in the command line.
vim.o.clipboard = "unnamedplus" -- Copy/Cut/Paste to system clipboard
vim.o.ignorecase = true -- Search case insensitive...
vim.o.smartcase = true -- ... but not if it contains caps
vim.o.cursorline = true -- Highlight current line
vim.o.guicursor = vim.o.guicursor .. ",t:ver25"
vim.o.fo = "jcql" -- See :help fo-table
vim.o.updatetime = 100 -- Faster completion
vim.o.laststatus = 3 -- Single Statusline for all windows
vim.o.timeoutlen = 300 -- Faster completion
vim.o.number = true -- Line numbers
vim.o.termguicolors = true -- Enable 24-bit RGB colors
vim.o.inccommand = "split" -- Show partial commands in the command line
vim.o.relativenumber = true -- Relative line numbers
vim.o.scrolloff = 10 -- Scroll when cursor is 8 lines away from screen edge
vim.o.list = true -- Show whitespace
vim.o.listchars = "tab:  ,trail:·,extends: ,precedes: ,eol:↲,conceal:┊,nbsp:␣"
vim.o.title = true
vim.o.titlestring = "nvim %M%F" -- Set title of the terminal.

---@enum FuzzyFinders
FuzzyFinders = {
  FZF = 1,
  Snacks = 2,
  Telescope = 3,
}

Options = {}
Options.FuzzyFinder = FuzzyFinders.Snacks
Options.Colorscheme = "gruvbuddy"
