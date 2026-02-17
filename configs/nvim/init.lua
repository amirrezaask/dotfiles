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
vim.diagnostic.config({ virtual_text = false })
vim.o.laststatus = 3
vim.opt.foldmethod = "expr"
vim.opt.foldcolumn = "0"
vim.opt.foldtext = ""
vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 5
vim.opt.foldnestmax = 4
vim.opt.cursorline = true

-- highlight yanked region for 150ms
vim.cmd([[ autocmd TextYankPost * silent! lua vim.hl.on_yank {higroup='Visual', timeout=150 } ]])

-- auto resize splits when the terminal's window is resized
vim.api.nvim_create_autocmd("VimResized", { command = "wincmd =", })
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


	-- LSP & Package management
	{ src = "https://github.com/mason-org/mason.nvim" },
	{ src = "https://github.com/mason-org/mason-lspconfig.nvim" },
	{ src = "https://github.com/neovim/nvim-lspconfig" },

	-- Colors
	{ src = "https://github.com/folke/tokyonight.nvim" },
	{ src = 'https://github.com/rose-pine/neovim' },
	{ src = 'https://github.com/catppuccin/nvim',                name = 'catppuccin' },
	{ src = 'https://github.com/vague-theme/vague.nvim' },


	-- Treesitter
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter" },

	{ src = 'https://github.com/nvim-lualine/lualine.nvim', },                      -- Statusline
	{ src = "https://github.com/stevearc/conform.nvim" },                           -- Autoformat
	{ src = "https://github.com/folke/snacks.nvim" },
	{ src = "https://github.com/saghen/blink.cmp",               version = "v1.6.0" }, -- Blazingly fast autocomplete popup

}

require('lualine').setup {}

require("blink.cmp").setup({
	completion = { list = { selection = { preselect = false } } },
	keymap = {
		preset = 'default',
		['<Tab>'] = { 'accept', 'fallback' },
		['<CR>'] = { 'accept', 'fallback' },
	},
})


require("tokyonight").setup {}

vim.cmd.colorscheme("catppuccin-macchiato")


vim.cmd [[
	hi Normal guibg=none
	hi NormalFloat guibg=none
]]

-- IDE [[
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
		vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf })
	end,
})

vim.lsp.config("lua_ls", { settings = { Lua = { workspace = { library = vim.api.nvim_get_runtime_file("", true) } } } })


require("mason").setup()
require("mason-lspconfig").setup({ ensure_installed = { "lua_ls", "gopls" } })

require("conform").setup({ -- Autoformat
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
-- ]]

require("snacks").setup {
	bigfile = { enabled = true },
	explorer = { enabled = true },
	indent = { enabled = true },
	input = { enabled = true },
	picker = { enabled = true },
	notifier = { enabled = true },
	quickfile = { enabled = true },
	scope = { enabled = true },
	statuscolumn = { enabled = true },
	words = { enabled = true },
}

vim.keymap.set("n", "<leader><leader>", Snacks.picker.files, { silent = true })
vim.keymap.set("n", "gd", Snacks.picker.lsp_definitions, { silent = true })
vim.keymap.set("n", "grr", Snacks.picker.lsp_references, { silent = true })
vim.keymap.set("n", "gri", Snacks.picker.lsp_implementations, { silent = true })
vim.keymap.set({ "n", "v" }, "<leader>j", Snacks.picker.grep, { silent = true })
vim.keymap.set({ "n", "v" }, "<leader>J", Snacks.picker.grep_word, { silent = true })



require("nvim-treesitter.configs").setup({ highlight = { enable = true }, auto_install = true })

vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()" -- use treesitter for code folding
