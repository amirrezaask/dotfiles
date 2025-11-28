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
-- vim.o.winborder = "rounded"
vim.o.scrolloff = 5
vim.o.splitkeep = "topline"
vim.o.linebreak = true
vim.o.completeopt = "menuone,noselect,noinsert,fuzzy"
vim.o.splitbelow = true
vim.o.splitright = true
vim.opt.wildoptions:append("fuzzy")
vim.diagnostic.config({ virtual_text = false })
vim.o.laststatus = 3

vim.cmd([[ autocmd TextYankPost * silent! lua vim.hl.on_yank {higroup='Visual', timeout=150 } ]])

vim.keymap.set("i", "jk", "<esc>")
vim.keymap.set("i", "kj", "<esc>")
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("n", "<leader>i", ":edit $MYVIMRC<CR>")
vim.keymap.set("n", "<CR>",
	function()
		if vim.v.hlsearch == 1 then
			vim.cmd.nohl()
			return ""
		else
			return vim.keycode "<CR>"
		end
	end, { expr = true })

vim.pack.add { -- See :h vim.pack
	{ src = "https://github.com/mason-org/mason.nvim" },
	{ src = "https://github.com/mason-org/mason-lspconfig.nvim" },
	{ src = "https://github.com/sainnhe/everforest" },
	{ src = "https://github.com/folke/tokyonight.nvim" },
	{ src = "https://github.com/neovim/nvim-lspconfig" },
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter" },
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter-context" },
	{ src = "https://github.com/stevearc/conform.nvim" },
	{ src = "https://github.com/folke/snacks.nvim" },
	{ src = "https://github.com/nvim-lualine/lualine.nvim" },
	{ src = "https://github.com/SmiteshP/nvim-navic" }

}
require('lualine').setup {
	sections = {
		lualine_a = { 'mode' },
		lualine_b = { 'branch', 'diff', 'diagnostics' },
		lualine_c = { 'filename', 'lsp_progress', 'navic' },
		lualine_x = { 'encoding', 'fileformat', 'filetype' },
		lualine_y = { 'progress' },
		lualine_z = { 'location' }
	},
}

vim.g.everforest_background = 'hard'
vim.cmd.colorscheme("tokyonight-storm")

-- Match background with ghostty Everforest Dark Hard
-- vim.api.nvim_set_hl(0, "Normal", { bg = "#1e2326" })
-- vim.api.nvim_set_hl(0, "NormalFloat", { bg = "#1e2326" })


-- Default Keybindings
-- see :h lsp-defaults
-- see :h vim.lsp.buf.tagfunc()
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		local client = vim.lsp.get_clients({ bufnr = args.buf })[1]
		if client then
			require("nvim-navic").attach(client, args.buf)
		end
		vim.lsp.completion.enable(true, client.id, args.buf, { autotrigger = true })
		vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
		vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf })
	end,
})

vim.lsp.config("lua_ls",
	{ settings = { Lua = { workspace = { library = vim.api.nvim_get_runtime_file("", true) } } } })

require("mason").setup()
require("nvim-navic").setup {}
require("mason-lspconfig").setup({ ensure_installed = { "lua_ls", "gopls" } })
require("conform").setup({
	formatters_by_ft = {
		php = nil,
		go = { "goimports" },
		lua = { "stylua" },
		json = { "jq" },
	},
	format_on_save = function(bufnr)
		if vim.bo[bufnr].filetype == "php" then
			return false
		end
		return { timeout_ms = 500, lsp_fallback = true }
	end,
})
require("snacks").setup({ picker = { enabled = true }, terminal = { enabled = true }, input = { enabled = true }, indent = { enabled = true } })
vim.keymap.set("n", "<leader><leader>", Snacks.picker.files, { silent = true })
vim.keymap.set("n", "<leader>j", Snacks.picker.grep, { silent = true })
vim.keymap.set({ "n", "v" }, "<leader>J", Snacks.picker.grep_word, { silent = true })


require("nvim-treesitter.configs").setup({ highlight = { enable = true }, auto_install = true })
require 'treesitter-context'.setup {}
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.opt.foldcolumn = "0"
vim.opt.foldtext = ""
vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 5
vim.opt.foldnestmax = 4

-- restore cursor to file position in previous editing session
vim.api.nvim_create_autocmd("BufReadPost", {
	callback = function(args)
		local mark = vim.api.nvim_buf_get_mark(args.buf, '"')
		local line_count = vim.api.nvim_buf_line_count(args.buf)
		if mark[1] > 0 and mark[1] <= line_count then
			vim.api.nvim_win_set_cursor(0, mark)
			-- defer centering slightly so it's applied after render
			vim.schedule(function()
				vim.cmd("normal! zz")
			end)
		end
	end,
})

-- auto resize splits when the terminal's window is resized
vim.api.nvim_create_autocmd("VimResized", {
	command = "wincmd =",
})


-- show cursorline only in active window enable
vim.api.nvim_create_autocmd({ "WinEnter", "BufEnter" }, {
	group = vim.api.nvim_create_augroup("active_cursorline", { clear = true }),
	callback = function()
		vim.opt_local.cursorline = true
	end,
})

-- show cursorline only in active window disable
vim.api.nvim_create_autocmd({ "WinLeave", "BufLeave" }, {
	group = "active_cursorline",
	callback = function()
		vim.opt_local.cursorline = false
	end,
})
