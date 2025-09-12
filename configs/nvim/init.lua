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
vim.o.list = true

vim.opt.listchars = {
  tab = '│ ',      -- Tab: pipe followed by space (fills tab width; use '│─' for dashes)
  lead = '·',      -- Leading spaces: single dot per space (unchanged)
  trail = '·',     -- Trailing spaces: dot (unchanged)
  extends = '…',   -- Line extends beyond window: ellipsis (unchanged)
  precedes = '…',  -- Line precedes window: ellipsis (unchanged)
  nbsp = '␣',      -- Non-breaking space: special char (unchanged)
  eol = '↲'        -- End of line: arrow (optional)
}
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

local map = vim.keymap.set

map("i", "jk", "<esc>")
map("i", "kj", "<esc>")
map("i", "<C-c>", "<esc>")
map("n", "<C-d>", "<C-d>zz")
map("n", "<C-u>", "<C-u>zz")
map("n", "n", "nzz")
map("n", "N", "Nzz")
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



local themefile = vim.fn.expand("~/.config/current-theme/neovim.theme")
local theme_name
local theme_plugin

if vim.fn.filereadable(themefile) then
	local themefile_contents = vim.fn.readfile(themefile)
	theme_name = themefile_contents[1]
	theme_plugin = themefile_contents[2]
end

if not theme_plugin then
	theme_plugin = "folke/tokyonight.nvim"
	theme_name = "tokyonight"
end

require("lazy").setup({
	theme_plugin, -- load theme from theme system
	{ -- AI
		"supermaven-inc/supermaven-nvim",
		config = function()
			require("supermaven-nvim").setup({})
		end,
	},
	{
		"ibhagwan/fzf-lua",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("fzf-lua").setup {
				"fzf-vim",
				keymap = {
					fzf = {
						["ctrl-q"] = "select-all+accept",
					},
				},
			}
			local FzfLua = require "fzf-lua"
			vim.keymap.set("n", "<leader><leader>", FzfLua.files)
			vim.keymap.set("n", "<leader>j", FzfLua.live_grep)
			vim.keymap.set({ "n" }, "<leader>k", FzfLua.grep_cword)
			vim.keymap.set({ "v" }, "<leader>k", FzfLua.grep_visual)
			vim.keymap.set("n", "<leader>O", FzfLua.lsp_workspace_symbols)

			vim.lsp.buf.references = FzfLua.lsp_references
			vim.lsp.buf.definition = FzfLua.lsp_definitions
			vim.lsp.buf.implementation = FzfLua.lsp_implementations
			vim.lsp.buf.document_symbol = FzfLua.lsp_symbols
			vim.lsp.buf.workspace_symbol = FzfLua.lsp_workspace_symbols
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

	{ "stevearc/oil.nvim", opts = {} },

	"tpope/vim-surround",
	"tpope/vim-unimpaired",

	{
		"nvim-tree/nvim-tree.lua",
		version = "*",
		lazy = false,
		dependencies = {
			"nvim-tree/nvim-web-devicons",
		},
		config = function()
			require("nvim-tree").setup({})
			vim.keymap.set("n", "<leader>e", ":NvimTreeToggle<CR>")
		end,
	},
	{ -- Code Autoformat
		"stevearc/conform.nvim",
		opts = {
			formatters_by_ft = {
				php = nil,
				go = { "goimports" },
				lua = { "stylua" },
			},
			format_on_save = function(bufnr)
				-- Skip formatting for PHP files
				if vim.bo[bufnr].filetype == "php" then
					return false
				end
				return { timeout_ms = 500, lsp_fallback = true }
			end,
		},
	},
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		opts = {

			options = {
				theme = "auto",
				globalstatus = vim.o.laststatus == 3,
				disabled_filetypes = { statusline = { "dashboard", "alpha", "ministarter", "snacks_dashboard" } },
			},
			sections = {
				lualine_a = { "mode" },
				lualine_b = { "branch" },

				lualine_c = {
					{ "filetype", icon_only = true, separator = "", padding = { left = 1, right = 0 } },
					{ 'filename', path = 1 },
				},
				lualine_x = { 'encoding', 'fileformat', 'filetype' },
				lualine_y = { 'progress' },
				lualine_z = { 'location' }
			},
			extensions = {},
		}
	},
	{
		"nvim-treesitter/nvim-treesitter",
		main = "nvim-treesitter.configs",
		opts = { highlight = { enable = true }, auto_install = true },
	},

	{ "saghen/blink.cmp",  version = "v1.6.0", opts = {} }
})

-- Start server to allow remote access
vim.fn.serverstart("/tmp/nvimsocket-" .. vim.fn.getpid())

vim.cmd("colorscheme " .. theme_name)
