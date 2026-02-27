-- ============================================================
-- Leader Keys
-- ============================================================
-- Set leader key to space and local leader to space
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- ============================================================
-- Package Manager: lazy.nvim
-- ============================================================
-- Bootstrap lazy.nvim if not already installed
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

-- ============================================================
-- Options
-- ============================================================
-- Persistence
vim.o.undofile = true -- Persist undo history across sessions
vim.o.swapfile = false -- Disable swap files

-- Editor UI
vim.o.number = true -- Show line numbers
vim.o.relativenumber = true -- Use relative line numbers
vim.o.signcolumn = "yes" -- Always show sign column
vim.o.cursorline = true -- Highlight current line
vim.o.scrolloff = 5 -- Keep 5 lines context when scrolling
vim.o.linebreak = true -- Wrap at word boundaries
vim.o.winborder = "rounded" -- Rounded borders for floating windows
vim.o.laststatus = 3 -- Global statusline

-- Folding (treesitter-based)
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "" -- Set by treesitter plugin
vim.opt.foldcolumn = "0"
vim.opt.foldtext = ""
vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 5
vim.opt.foldnestmax = 4

-- Indentation
vim.o.tabstop = 4 -- Tab width
vim.o.shiftwidth = 0 -- Use tabstop value

-- Search
vim.o.ignorecase = true -- Case-insensitive search
vim.o.smartcase = true -- Case-sensitive if uppercase in pattern

-- Editing
vim.o.formatoptions = "jcql" -- Auto-formatting options
vim.o.inccommand = "split" -- Preview substitutions in split
vim.o.completeopt = "menuone,noselect,noinsert,fuzzy" -- Completion behavior

-- Splits
vim.o.splitbelow = true -- Horizontal splits go below
vim.o.splitright = true -- Vertical splits go right
vim.o.splitkeep = "topline" -- Keep cursor line when splitting

-- Clipboard
vim.o.clipboard = "unnamedplus" -- Use system clipboard

-- Wildmenu
vim.opt.wildoptions:append("fuzzy") -- Fuzzy completion in command line

-- Diagnostics
vim.diagnostic.config({ virtual_text = false }) -- Show diagnostics in floating window only

-- ============================================================
-- Autocommands
-- ============================================================
-- Highlight yanked text briefly
vim.api.nvim_create_autocmd("TextYankPost", {
	callback = function()
		vim.hl.on_yank({ higroup = "Visual", timeout = 150 })
	end,
})

-- Equalize windows on resize
vim.api.nvim_create_autocmd("VimResized", { command = "wincmd =" })

-- Restore cursor position when reopening file
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

-- ============================================================
-- Keymaps
-- ============================================================
-- Escape alternatives in insert mode
vim.keymap.set("i", "jk", "<esc>")
vim.keymap.set("i", "kj", "<esc>")
vim.keymap.set("i", "<C-c>", "<esc>")

-- Center screen when scrolling
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

-- Center screen when searching
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")

-- Move by visual lines (for wrapped text)
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")

-- Quick edit config
vim.keymap.set("n", "<leader>i", ":edit $MYVIMRC<CR>")

-- Clear search highlight with Enter
vim.keymap.set("n", "<CR>", function()
	if vim.v.hlsearch == 1 then
		vim.cmd.nohl()
		return ""
	else
		return vim.keycode("<CR>")
	end
end, { expr = true })

-- ============================================================
-- Plugins
-- ============================================================
require("lazy").setup({
	-- --------------------------------------------------------
	-- Colorschemes
	-- --------------------------------------------------------
	-- Tokyo Night: Dark theme with vibrant colors
	{
		"folke/tokyonight.nvim",
		priority = 1000,
		opts = { transparent = true },
	},
	-- Rose Pine: Soft, natural color palette
	{
		"rose-pine/neovim",
		name = "rose-pine",
		opts = { styles = { transparency = true } },
	},
	-- Catppuccin: Pastel theme with multiple variants
	{
		"catppuccin/nvim",
		name = "catppuccin",
		opts = { transparent = true },
	},
	-- Vague: Minimal, low-contrast theme
	{
		"vague-theme/vague.nvim",
		opts = { transparent = true },
	},

	-- --------------------------------------------------------
	-- Completion: blink.cmp
	-- --------------------------------------------------------
	-- Fast, modern completion engine with LSP integration
	-- Supports multiple sources: LSP, paths, buffers, snippets
	{
		"saghen/blink.cmp",
		version = "v1.6.0",
		opts = {
			sources = {
				default = { "lsp", "path", "buffer", "snippets" },
			},
			completion = { list = { selection = { preselect = false } } },
			keymap = {
				preset = "default",
				["<Tab>"] = { "accept", "fallback" },
				["<CR>"] = { "accept", "fallback" },
			},
		},
	},

	-- --------------------------------------------------------
	-- LSP: mason + nvim-lspconfig
	-- --------------------------------------------------------
	-- Mason: Package manager for LSP servers, linters, formatters
	{
		"mason-org/mason.nvim",
		opts = {},
	},
	-- Mason-lspconfig: Bridge between mason and nvim-lspconfig
	{
		"mason-org/mason-lspconfig.nvim",
		dependencies = { "mason-org/mason.nvim", "neovim/nvim-lspconfig" },
		opts = {
			ensure_installed = { "lua_ls", "gopls" },
		},
	},
	-- nvim-lspconfig: LSP client configuration
	-- Sets up LSP keymaps and server-specific settings
	{
		"neovim/nvim-lspconfig",
		config = function()
			-- LSP keymaps on attach
			vim.api.nvim_create_autocmd("LspAttach", {
				callback = function(args)
					vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
					vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf })
				end,
			})
			-- Lua language server settings
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

	-- --------------------------------------------------------
	-- Formatting: conform.nvim
	-- --------------------------------------------------------
	-- Async formatter with support for multiple formatters per filetype
	-- Auto-formats on save with LSP fallback
	{
		"stevearc/conform.nvim",
		opts = {
			formatters_by_ft = {
				php = nil,
				go = { "goimports" },
				lua = { "stylua" },
				json = { "jq" },
				javascript = { "eslint_d" },
				typescript = { "eslint_d" },
				javascriptreact = { "eslint_d" },
				typescriptreact = { "eslint_d" },
			},
			format_on_save = function(bufnr)
				local ft = vim.bo[bufnr].filetype
				if
					({
						php = true,
						javascript = true,
						typescript = true,
						javascriptreact = true,
						typescriptreact = true,
					})[ft]
				then
					return { timeout_ms = 500, lsp_fallback = false }
				end
				return { timeout_ms = 500, lsp_fallback = true }
			end,
		},
	},

	-- --------------------------------------------------------
	-- Syntax: treesitter
	-- --------------------------------------------------------
	-- Better syntax highlighting and code understanding
	-- Also used for folding
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

	-- --------------------------------------------------------
	-- Status Line: lualine
	-- --------------------------------------------------------
	-- Fast, customizable statusline with sections for:
	-- mode, branch, diff, diagnostics, filename, encoding, file type
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

	-- --------------------------------------------------------
	-- Buffer Line
	-- --------------------------------------------------------
	-- Visual buffer tabs (currently disabled)
	{
		"akinsho/bufferline.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		enabled = false,
		opts = {
			options = {
				show_buffer_icons = true,
				show_buffer_close_icons = false,
				show_close_icon = false,
			},
		},
	},

	-- --------------------------------------------------------
	-- Utils: snacks.nvim
	-- --------------------------------------------------------
	-- Collection of utility plugins including:
	-- - picker: Fuzzy finder for files, grep, LSP symbols
	-- - dashboard: Start screen
	-- - notifier: Notification manager
	-- - bigfile: Handle large files gracefully
	-- - indent: Indent guides
	-- - quickfile: Fast file loading
	-- - statuscolumn: Custom status column
	{
		"folke/snacks.nvim",
		priority = 1000,
		lazy = false,
		dependencies = { "nvim-tree/nvim-web-devicons" },
		---@type snacks.Config
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
			-- Find files in current directory
			{
				"<leader><leader>",
				function()
					Snacks.picker.files()
				end,
				mode = "n",
				silent = true,
			},
			-- Find files in git repo
			{
				"<leader>pf",
				function()
					Snacks.picker.git_files()
				end,
				mode = "n",
				silent = true,
			},
			-- Go to definition
			{
				"gd",
				function()
					Snacks.picker.lsp_definitions()
				end,
				mode = "n",
				silent = true,
			},
			-- Find references
			{
				"grr",
				function()
					Snacks.picker.lsp_references()
				end,
				mode = "n",
				silent = true,
			},
			-- Go to implementation
			{
				"gri",
				function()
					Snacks.picker.lsp_implementations()
				end,
				mode = "n",
				silent = true,
			},
			-- Live grep
			{
				"<leader>j",
				function()
					Snacks.picker.grep()
				end,
				mode = { "n", "v" },
				silent = true,
			},
			-- Grep word under cursor
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

	-- --------------------------------------------------------
	-- Icons
	-- --------------------------------------------------------
	-- File icons used by lualine, bufferline, snacks, etc.
	{ "nvim-tree/nvim-web-devicons" },
})

-- ============================================================
-- Colorscheme
-- ============================================================
vim.cmd([[ colorscheme tokyonight-night ]])
