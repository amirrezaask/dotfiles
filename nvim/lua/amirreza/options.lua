local o = vim.o
o.wrap = true -- Wrap long lines.
o.breakindent = true -- Indent wrapped lines.
o.signcolumn = "yes" -- Show signcolumn.
o.swapfile = false -- Disable swapfile.
o.undofile = true -- Store undo history on disk
o.splitbelow = true -- Split windows below the current windows
o.splitright = true -- Split windows right to the current windows
o.showmode = false -- Don't show Vim mode in the command line.
o.clipboard = "unnamedplus" -- Copy/Cut/Paste to system clipboard
o.ignorecase = true -- Search case insensitive...
o.smartcase = true -- ... but not if it contains caps
o.cursorline = true -- Highlight current line
o.guicursor = o.guicursor .. ",t:ver25"
o.updatetime = 100 -- Faster completion
o.laststatus = 3 -- Single Statusline for all windows
o.timeoutlen = 300 -- Faster completion
o.number = true -- Line numbers
o.termguicolors = true -- Enable 24-bit RGB colors
-- o.winborder = "rounded" -- All floating windows will have rounded borders
o.inccommand = "split" -- Show partial commands in the command line
o.relativenumber = true -- Relative line numbers
o.scrolloff = 10 -- Scroll when cursor is 8 lines away from screen edge
o.list = true -- Show whitespace
o.listchars = "tab:  ,trail:·,extends: ,precedes: ,eol:↲,conceal:┊,nbsp:␣"

---@enum FuzzyFinders
FuzzyFinders = {
  FZF = 1,
  Snacks = 2,
  Telescope = 3,
}

-- local fuzzy_finder = fuzzy_finders.FZF_lua
FuzzyFinder = FuzzyFinders.Snacks
