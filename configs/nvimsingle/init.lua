-- ╔════════════════════════════════════════════════════════════════════════════╗
-- ║                           NEOVIM CONFIGURATION                              ║
-- ║                        Single File Configuration                            ║
-- ╚════════════════════════════════════════════════════════════════════════════╝

-- ─────────────────────────────────────────────────────────────────────────────
-- LEADER KEYS
-- ─────────────────────────────────────────────────────────────────────────────
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- ─────────────────────────────────────────────────────────────────────────────
-- PACKAGE MANAGER: lazy.nvim
-- ─────────────────────────────────────────────────────────────────────────────
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
local uv = vim.uv or vim.loop
if not uv.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable",
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

-- ─────────────────────────────────────────────────────────────────────────────
-- OPTIONS
-- ─────────────────────────────────────────────────────────────────────────────
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

-- ─────────────────────────────────────────────────────────────────────────────
-- KEYMAPS
-- ─────────────────────────────────────────────────────────────────────────────
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
vim.keymap.set("n", "<CR>", function()
	if vim.v.hlsearch == 1 then
		vim.cmd.nohl()
		return ""
	else
		return vim.keycode("<CR>")
	end
end, { expr = true })

-- ─────────────────────────────────────────────────────────────────────────────
-- AUTOCMDS
-- ─────────────────────────────────────────────────────────────────────────────
vim.api.nvim_create_autocmd("TextYankPost", {
	callback = function()
		vim.hl.on_yank({ higroup = "Visual", timeout = 150 })
	end,
})

vim.api.nvim_create_autocmd("VimResized", { command = "wincmd =" })

vim.api.nvim_create_autocmd("BufReadPost", {
	callback = function(args)
		local mark = vim.api.nvim_buf_get_mark(args.buf, '"')
		local line_count = vim.api.nvim_buf_line_count(args.buf)
		if mark[1] > 0 and mark[1] <= line_count then
			vim.api.nvim_win_set_cursor(0, mark)
			vim.schedule(function()
				vim.cmd("normal! zz")
			end)
		end
	end,
})

-- ─────────────────────────────────────────────────────────────────────────────
-- PLUGINS
-- ─────────────────────────────────────────────────────────────────────────────
require("lazy").setup({
	-- ─────────────────────────────────────────────────────────────────────────
	-- COLORSHEMES
	-- ─────────────────────────────────────────────────────────────────────────
	{
		"folke/tokyonight.nvim",
		priority = 1000,
		config = function()
			require("tokyonight").setup({ transparent = true })
			vim.cmd.colorscheme("tokyonight-night")
		end,
	},
	{ "rose-pine/neovim",      name = "rose-pine" },
	{ "catppuccin/nvim",       name = "catppuccin" },
	{ "vague-theme/vague.nvim" },

	-- ─────────────────────────────────────────────────────────────────────────
	-- MASON: External Tool Manager (LSP servers, DAP, linters, formatters)
	-- ─────────────────────────────────────────────────────────────────────────
	{ "mason-org/mason.nvim",  opts = {} },
	{
		"mason-org/mason-lspconfig.nvim",
		dependencies = { "mason-org/mason.nvim", "neovim/nvim-lspconfig" },
		opts = {
			ensure_installed = { "lua_ls", "gopls" },
		},
	},

	-- ─────────────────────────────────────────────────────────────────────────
	-- TREESITTER: Syntax Highlighting & Folding
	-- ─────────────────────────────────────────────────────────────────────────
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			require("nvim-treesitter.configs").setup({
				highlight = { enable = true },
				auto_install = true,
			})
			vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
		end,
	},

	-- ─────────────────────────────────────────────────────────────────────────
	-- LUALINE: Statusline
	-- ─────────────────────────────────────────────────────────────────────────
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		opts = {
			sections = {
				lualine_a = { "mode" },
				lualine_b = { "branch", "diff", "diagnostics" },
				lualine_c = { { "filename", path = 1 } },
				lualine_x = { "encoding", "fileformat", "filetype" },
				lualine_y = { "progress" },
				lualine_z = { "location" },
			},
		},
	},

	-- ─────────────────────────────────────────────────────────────────────────
	-- BUFFERLINE: Tabline
	-- ─────────────────────────────────────────────────────────────────────────
	{
		"akinsho/bufferline.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		opts = {
			options = {
				show_buffer_icons = true,
				show_buffer_close_icons = false,
				show_close_icon = false,
			},
		},
	},

	-- ─────────────────────────────────────────────────────────────────────────
	-- CONFORM: Code Formatter
	-- ─────────────────────────────────────────────────────────────────────────
	{
		"stevearc/conform.nvim",
		opts = {
			formatters_by_ft = {
				php = nil,
				go = { "goimports" },
				lua = { "stylua" },
				json = { "jq" },
				javascript = { "eslint" },
				javascriptreact = { "eslint" },
				typescript = { "eslint" },
				typescriptreact = { "eslint" },
			},
			format_on_save = function(bufnr)
				if vim.bo[bufnr].filetype == "php" then
					return false
				end
				return { timeout_ms = 500, lsp_fallback = true }
			end,
		},
	},

	-- ─────────────────────────────────────────────────────────────────────────
	-- LSPCONFIG: Language Server Protocol Configuration
	-- ─────────────────────────────────────────────────────────────────────────
	{
		"neovim/nvim-lspconfig",
		config = function()
			vim.api.nvim_create_autocmd("LspAttach", {
				callback = function(args)
					vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
					vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf })
				end,
			})
			vim.lsp.config("lua_ls", {
				settings = {
					Lua = {
						diagnostics = {
							globals = { "vim" },
						},
						workspace = { library = vim.api.nvim_get_runtime_file("", true) },
					},
				},
			})
		end,
	},

	-- ─────────────────────────────────────────────────────────────────────────
	-- BLINK.CMP: Autocompletion Engine
	-- ─────────────────────────────────────────────────────────────────────────
	{
		"saghen/blink.cmp",
		version = "v1.6.0",
		opts = {
			completion = { list = { selection = { preselect = false } } },
			keymap = {
				preset = "default",
				["<Tab>"] = { "accept", "fallback" },
				["<CR>"] = { "accept", "fallback" },
			},
		},
	},

	-- ─────────────────────────────────────────────────────────────────────────
	-- SNACKS: Collection of Small QoL Plugins
	-- ─────────────────────────────────────────────────────────────────────────
	{
		"folke/snacks.nvim",
		priority = 1000,
		lazy = false,
		dependencies = { "nvim-tree/nvim-web-devicons" },
		opts = {
			bigfile = { enabled = true },
			indent = { enabled = true },
			picker = { enabled = true },
			notifier = { enabled = true },
			quickfile = { enabled = true },
			statuscolumn = { enabled = true },
			dashboard = { enabled = true },
		},
		keys = {
			{
				"<leader><leader>",
				function()
					Snacks.picker.files()
				end,
				mode = "n",
				silent = true,
			},
			{
				"<leader>pf",
				function()
					Snacks.picker.git_files()
				end,
				mode = "n",
				silent = true,
			},
			{
				"gd",
				function()
					Snacks.picker.lsp_definitions()
				end,
				mode = "n",
				silent = true,
			},
			{
				"grr",
				function()
					Snacks.picker.lsp_references()
				end,
				mode = "n",
				silent = true,
			},
			{
				"gri",
				function()
					Snacks.picker.lsp_implementations()
				end,
				mode = "n",
				silent = true,
			},
			{
				"<leader>j",
				function()
					Snacks.picker.grep()
				end,
				mode = { "n", "v" },
				silent = true,
			},
			{
				"<leader>J",
				function()
					Snacks.picker.grep_word()
				end,
				mode = { "n", "v" },
				silent = true,
			},
		},
	},
})
