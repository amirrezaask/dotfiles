COLORSCHEME = "tokyonight"
TRANSPARENT = true

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

local map = vim.keymap.set

map("i", "jk", "<esc>")
map("i", "kj", "<esc>")
map("i", "<C-c>", "<esc>")
map("n", "<C-d>", "<C-d>zz")
map("n", "<C-u>", "<C-u>zz")
map("n", "n", "nzz")
map("n", "N", "Nzz")
map("n", "j", "gj")
map("n", "k", "gk")
map("n", "<C-j>", "<C-w>j")
map("n", "<C-k>", "<C-w>k")
map("n", "<C-h>", "<C-w>h")
map("n", "<C-l>", "<C-w>l")
map('t', '<Esc>', '<C-\\><C-n>', { noremap = true, silent = true })
map('t', '<C-j>', '<C-\\><C-n><C-w>j', { noremap = true, silent = true })
map('t', '<C-k>', '<C-\\><C-n><C-w>k', { noremap = true, silent = true })
map('t', '<C-h>', '<C-\\><C-n><C-w>h', { noremap = true, silent = true })
map('t', '<C-l>', '<C-\\><C-n><C-w>l', { noremap = true, silent = true })
map("n", "<CR>", function()
	---@diagnostic disable-next-line: undefined-field
	if vim.v.hlsearch == 1 then
		vim.cmd.nohl()
		return ""
	else
		return vim.keycode "<CR>"
	end
end, { expr = true })

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
	if vim.v.shell_error ~= 0 then
		vim.api.nvim_echo({
			{ "Failed to clone lazy.nvim:\n", "ErrorMsg" },
			{ out,                            "WarningMsg" },
			{ "\nPress any key to exit..." },
		}, true, {})
		vim.fn.getchar()
		os.exit(1)
	end
end

vim.opt.rtp:prepend(lazypath)

require("lazy").setup({

	{ "stevearc/oil.nvim", opts = {} },

	"tpope/vim-surround",
	"tpope/vim-unimpaired",

	{ -- AI autocomplete
		"supermaven-inc/supermaven-nvim",
		config = function()
			require("supermaven-nvim").setup({})
		end,
	},

	{
		"mason-org/mason-lspconfig.nvim",
		opts = {
			ensure_installed = { "lua_ls", "gopls" },
		},
		dependencies = {
			{ "mason-org/mason.nvim", opts = {} },
			{ "folke/trouble.nvim",   opts = {} },
			{
				"neovim/nvim-lspconfig",
				config = function()
					-- Default Keybindings
					-- see :h lsp-defaults
					-- see :h vim.lsp.buf.tagfunc()
					vim.api.nvim_create_autocmd("LspAttach", {
						callback = function(args)
							-- local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
							-- if client:supports_method('textDocument/completion') then
							--   vim.lsp.completion.enable(true, client.id, args.buf, {autotrigger = true})
							-- end
							vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
							vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf })
							vim.keymap.set("n", "<leader>E", ":Trouble diagnostics<CR>", { buffer = args.buf })
						end,
					})
					vim.lsp.config(
						"lua_ls",
						{ settings = { Lua = { workspace = { library = vim.api.nvim_get_runtime_file("", true) } } } }
					)
				end,
			},
		},
	},
	{
		-- Code Autoformat
		"stevearc/conform.nvim",
		opts = {
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
		},
	},
	{
		"folke/snacks.nvim",
		config = function()
			require("snacks").setup {
				terminal = { enabled = true },
				indent = { enabled = true },
				picker = { enabled = true },
				input = { enabled = true },
				lazygit = { enabled = true },
			}

			local Snacks = require "snacks"
			local picker = Snacks.picker
			vim.keymap.set("n", "<leader><leader>", picker.files)
			vim.keymap.set("n", "<leader>j", picker.grep)
			vim.keymap.set({ "n", "v" }, "<leader>k", picker.grep_word)
			vim.keymap.set("n", "<leader>o", picker.lsp_symbols)
			vim.keymap.set("n", "<leader>O", picker.lsp_workspace_symbols)
			vim.keymap.set("n", "<leader>g", Snacks.lazygit.open)
			vim.keymap.set({ "n", "t" }, "<C-j>", Snacks.terminal.toggle)

			vim.lsp.buf.references = picker.lsp_references
			vim.lsp.buf.definition = picker.lsp_definitions
			vim.lsp.buf.implementation = picker.lsp_implementations
			vim.lsp.buf.document_symbol = picker.lsp_symbols
			vim.lsp.buf.workspace_symbol = picker.lsp_workspace_symbols
		end,
	},
	{ "saghen/blink.cmp",  version = "v1.6.0", opts = {} },
	{
		"nvim-treesitter/nvim-treesitter",
		main = "nvim-treesitter.configs",
		opts = { highlight = { enable = true }, auto_install = true },
	},
	{
		"folke/tokyonight.nvim",
		enabled = COLORSCHEME == "tokyonight",
		config = function()
			require("tokyonight").setup({
				style = "night",
				transparent = TRANSPARENT,
			})
			vim.cmd("colorscheme tokyonight-night")
		end,
	},
	{
		"catppuccin/nvim",
		name = "catppuccin",
		enabled = COLORSCHEME == "catppuccin",
		config = function()
			vim.cmd("colorscheme catppuccin-macchiato")
		end,
	},


})
