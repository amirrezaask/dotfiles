-- ============================================================
-- Leader Keys
-- ============================================================
vim.g.mapleader = " "
vim.g.maplocalleader = " "
K = vim.keymap.set

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

-- Autocomplete
vim.o.autocomplete = true
vim.o.pumborder = "rounded"
vim.o.pummaxwidth = 40

require("vim._core.ui2").enable({ enable = true })

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
K("i", "jk", "<esc>")
K("i", "kj", "<esc>")
K("i", "<C-c>", "<esc>")

-- Center screen when scrolling
K("n", "<C-d>", "<C-d>zz")
K("n", "<C-u>", "<C-u>zz")

-- Center screen when searching
K("n", "n", "nzz")
K("n", "N", "Nzz")

-- Move by visual lines (for wrapped text)
K("n", "j", "gj")
K("n", "k", "gk")

-- Quick edit config
K("n", "<leader>i", ":edit $MYVIMRC<CR>")

-- Clear search highlight with Enter
K("n", "<CR>", function()
	if vim.v.hlsearch == 1 then
		vim.cmd.nohl()
		return ""
	else
		return vim.keycode("<CR>")
	end
end, { expr = true })

-- ============================================================
-- Package Manager: vim.pack
-- ============================================================
vim.api.nvim_create_autocmd("PackChanged", { -- Updating Treesitter parsers if plugin updates.
	callback = function(event)
		if event.data.spec.name ~= "nvim-treesitter" then
			return
		end
		if event.data.kind == "install" or event.data.kind == "update" then
			vim.cmd("TSUpdate")
		end
	end,
})
local gh = function(repo)
	return "https://github.com/" .. repo
end

vim.pack.add({
	-- --------------------------------------------------------
	-- Colorschemes
	-- --------------------------------------------------------
	-- gh("folke/tokyonight.nvim"),
	-- { src = gh("rose-pine/neovim"), name = "rose-pine" },
	-- { src = gh("catppuccin/nvim"), name = "catppuccin" },
	-- gh("vague-theme/vague.nvim"),
	-- gh("navarasu/onedark.nvim"),
	-- gh("AlexvZyl/nordic.nvim"),
	-- gh("sainnhe/everforest"),

	-- --------------------------------------------------------
	-- LSP: mason + nvim-lspconfig
	-- --------------------------------------------------------
	gh("mason-org/mason.nvim"),
	gh("mason-org/mason-lspconfig.nvim"),
	gh("neovim/nvim-lspconfig"),

	-- --------------------------------------------------------
	-- Formatting: conform.nvim
	-- --------------------------------------------------------
	gh("stevearc/conform.nvim"),

	-- --------------------------------------------------------
	-- Syntax: treesitter
	-- --------------------------------------------------------
	gh("nvim-treesitter/nvim-treesitter"),

	-- --------------------------------------------------------
	-- Status Line: lualine
	-- --------------------------------------------------------
	gh("nvim-lualine/lualine.nvim"),

	gh("stevearc/oil.nvim"),

	-- --------------------------------------------------------
	-- Utils: snacks.nvim
	-- --------------------------------------------------------
	{ src = gh("folke/snacks.nvim"), version = "main" },

	gh("ibhagwan/fzf-lua"),

	-- --------------------------------------------------------
	-- Icons
	-- --------------------------------------------------------
	gh("nvim-tree/nvim-web-devicons"),
}, { confirm = false, load = true })

-- Setting up colorschemes
-- require("tokyonight").setup({ transparent = true })
-- require("rose-pine").setup({ styles = { transparency = true } })
-- require("catppuccin").setup({ transparent = true })
-- require("vague").setup({ transparent = true })
-- require("onedark").setup({ style = "darker", transparent = true })
-- require("nordic").setup({})

-- vim.cmd([[ colorscheme everforest ]])
-- vim.cmd([[ hi! Normal guibg=#1e2326 ]])

-- Fzf
FzfLua = require("fzf-lua")
FzfLua.setup({ "ivy" })
K("n", "<leader><leader>", FzfLua.files)
K("n", "<leader>pf", FzfLua.git_files)
K("n", "<leader>gd", FzfLua.lsp_definitions)
K("n", "<leader>grr", FzfLua.lsp_references)
K("n", "<leader>gri", FzfLua.lsp_implementations)
K("n", "<leader>j", FzfLua.live_grep)
K({ "n", "v" }, "<leader>j", FzfLua.grep_cword)

-- LSP setup
require("mason").setup({})
require("mason-lspconfig").setup({ ensure_installed = { "lua_ls", "gopls" } })

-- LSP keymaps on attach
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		K("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
		K("n", "L", vim.diagnostic.open_float, { buffer = args.buf })
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

-- Autoformat on save
require("conform").setup({
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
})

require("nvim-treesitter.configs").setup({ highlight = { enable = true }, auto_install = true })
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
