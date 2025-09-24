vim.g.mapleader = " "
vim.o.undofile = true

vim.o.signcolumn = "yes"
vim.o.number = true
vim.o.relativenumber = true
vim.o.swapfile = false
vim.o.clipboard = "unnamedplus"
vim.o.tabstop = 4
vim.o.shiftwidth = 0
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.formatoptions = "jcql"
vim.o.inccommand = "split"
vim.o.winborder = "rounded"
vim.o.scrolloff = 5
vim.o.splitkeep = "topline"
vim.o.linebreak = true
vim.o.completeopt = "menuone,noselect,noinsert,fuzzy"
vim.o.splitbelow = true
vim.o.splitright = true
vim.opt.wildoptions:append("fuzzy")
vim.o.cursorline = true
vim.diagnostic.config({ virtual_text = true })
vim.o.list = true
vim.o.winbar = '%f %h%w%m%r'
vim.o.laststatus = 0 -- Disable status line

vim.opt.listchars = {
	tab = '│ ', -- Tab: pipe followed by space (fills tab width; use '│─' for dashes)
	lead = '·', -- Leading spaces: single dot per space (unchanged)
	trail = '·', -- Trailing spaces: dot (unchanged)
	extends = '…', -- Line extends beyond window: ellipsis (unchanged)
	precedes = '…', -- Line precedes window: ellipsis (unchanged)
	nbsp = '␣', -- Non-breaking space: special char (unchanged)
	eol = '↲' -- End of line: arrow (optional)
}

vim.cmd([[ autocmd TextYankPost * silent! lua vim.hl.on_yank {higroup='Visual', timeout=150 } ]])

-- vim.api.nvim_create_autocmd("ColorScheme", {
-- 	callback = function()
-- 		vim.cmd([[
-- 			hi Normal guibg=NONE
-- 			hi NormalFloat guibg=NONE
-- 			hi SignColumn guibg=NONE
-- 		]])
-- 	end,
-- })
