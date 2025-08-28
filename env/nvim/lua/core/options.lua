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
vim.o.laststatus = 3
vim.opt.wildoptions:append("fuzzy")
vim.o.cursorline = true
vim.diagnostic.config({ virtual_text = true })
vim.g.lazyvim_check_order = false
vim.cmd([[ autocmd TextYankPost * silent! lua vim.hl.on_yank {higroup='Visual', timeout=150 } ]])

vim.api.nvim_create_autocmd("ColorScheme", {
	callback = function()
		vim.cmd([[
			hi Normal guibg=NONE
			hi NormalFloat guibg=NONE
			hi SignColumn guibg=NONE
		]])
	end,
})
