vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.o.undofile = true -- Persist undo history across sessions
vim.o.swapfile = false -- Disable swap files
vim.o.number = true -- Show line numbers
vim.o.relativenumber = true -- Use relative line numbers
vim.o.signcolumn = "yes" -- Always show sign column
vim.o.cursorline = true -- Highlight current line
vim.o.scrolloff = 5 -- Keep 5 lines context when scrolling
vim.o.linebreak = true -- Wrap at word boundaries
vim.o.winborder = "rounded" -- Rounded borders for floating windows
vim.o.laststatus = 3 -- Global statusline

vim.o.foldmethod = "expr"
vim.o.foldexpr = "v:lua.vim.treesitter.foldexpr()" -- Set by treesitter plugin
vim.o.foldcolumn = "0"
vim.o.foldtext = ""
vim.o.foldlevel = 99
vim.o.foldlevelstart = 5
vim.o.foldnestmax = 4

-- vim.o.winbar = "%f %l:%c"
vim.o.laststatus = 3

vim.o.tabstop = 4 -- Tab width
vim.o.shiftwidth = 0 -- Use tabstop value

vim.o.ignorecase = true -- Case-insensitive search
vim.o.smartcase = true -- Case-sensitive if uppercase in pattern

vim.o.formatoptions = "jcql" -- Auto-formatting options
vim.o.inccommand = "split" -- Preview substitutions in split
vim.o.completeopt = "menuone,noselect,noinsert,fuzzy" -- Completion behavior

vim.o.splitbelow = true -- Horizontal splits go below
vim.o.splitright = true -- Vertical splits go right
vim.o.splitkeep = "topline" -- Keep cursor line when splitting

vim.o.clipboard = "unnamedplus" -- Use system clipboard

vim.opt.wildoptions:append("fuzzy") -- Fuzzy completion in command line

vim.diagnostic.config({ virtual_text = false }) -- Show diagnostics in floating window only

vim.o.autocomplete = true -- :h 'autocomplete', neovim 0.12+
vim.o.pumheight = 10
vim.o.pumblend = 10
--
local ok, ui2 = pcall(require, "vim._core.ui2") -- EXPERIMENTAL: Neovim 0.12 new UI
if ok then
	ui2.enable({ enable = true })
end

vim.api.nvim_create_autocmd("TextYankPost", { -- Highlight yanked text briefly
	callback = function()
		vim.hl.on_yank({ higroup = "Visual", timeout = 150 })
	end,
})

vim.api.nvim_create_autocmd("BufEnter", {
	-- pattern = "prompt",
	callback = function(args)
		if vim.bo[args.buf].buftype == "prompt" then
			vim.bo[args.buf].autocomplete = false
		end
	end,
})

vim.api.nvim_create_autocmd("VimResized", { command = "wincmd =" }) -- Equalize windows on resize

vim.api.nvim_create_autocmd("BufReadPost", { -- Restore cursor position when reopening file
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

vim.keymap.set("i", "jk", "<esc>")
vim.keymap.set("i", "kj", "<esc>")
vim.keymap.set("i", "<C-c>", "<esc>")

vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")

vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")

vim.keymap.set("n", "<leader>i", ":edit $MYVIMRC<CR>", { desc = "Edit Configuration" })

vim.keymap.set("i", "<C-Space>", "<C-x><C-o>", { desc = "Trigger LSP completion" })
vim.keymap.set("n", "<CR>", function() -- Clear search highlight with Enter
	if vim.v.hlsearch == 1 then
		vim.cmd.nohl()
		return ""
	else
		return vim.keycode("<CR>")
	end
end, { expr = true })

vim.pack.add({
	-- Themes
	"https://github.com/folke/tokyonight.nvim",
	"https://github.com/vague-theme/vague.nvim",
	{ src = "https://github.com/catppuccin/nvim", name = "catppuccin" },
	{ src = "https://github.com/embark-theme/vim", name = "embark" },
	{ src = "https://github.com/rose-pine/neovim", name = "rose-pine" },
	"https://github.com/sainnhe/everforest",
	"https://github.com/ellisonleao/gruvbox.nvim",

	-- LSP
	"https://github.com/mason-org/mason.nvim",
	"https://github.com/mason-org/mason-lspconfig.nvim",
	"https://github.com/neovim/nvim-lspconfig",

	"https://github.com/stevearc/conform.nvim", -- Autoformatting

	"https://github.com/stevearc/oil.nvim", -- File management

	"https://github.com/nvim-treesitter/nvim-treesitter", -- Syntax Highlighting

	"https://github.com/folke/snacks.nvim", -- Qol Plugins by folke ( Fuzzy Finder specificaly )

	-- Nice UI
	"https://github.com/MunifTanjim/nui.nvim",
	"https://github.com/folke/noice.nvim",

	"https://github.com/folke/which-key.nvim",

	"https://github.com/nvim-tree/nvim-web-devicons",
	"https://github.com/nvim-lualine/lualine.nvim",
}, { confirm = false, load = true })

require("vague").setup({
	transparent = false, -- If true, background is not set
	bold = false, -- Disable bold globally
	italic = false, -- Disable italic globally
})

vim.g.everforest_background = "hard"
vim.api.nvim_create_autocmd("ColorScheme", {
	pattern = "everforest",
	callback = function()
		vim.cmd([[
			hi! Normal guibg=#1e2326
			hi! NormalFloat guibg=#1e2326
			hi! Terminal guibg=#1e2326
		]])
	end,
})

require("gruvbox").setup({
	undercurl = false,
	underline = false,
	bold = false,
	italic = {
		strings = false,
		emphasis = false,
		comments = false,
		operators = false,
		folds = false,
	},
	contrast = "hard",
})

vim.cmd.colorscheme("tokyonight-moon")

require("oil").setup({})

require("lualine").setup({
	sections = {
		lualine_a = { "mode" },
		lualine_b = { "branch", "diff", "diagnostics" },
		lualine_c = { { "filename", path = 1 } },
		lualine_x = { "encoding", "fileformat", "filetype" },
		lualine_y = { "progress" },
		lualine_z = { "location" },
	},
})

require("which-key").setup({
	preset = "helix",
	loop = true,
})

require("snacks").setup({
	bigfile = { enabled = true },
	indent = { enabled = true },
	input = { enabled = true },
	picker = { enabled = true },
	notifier = { enabled = true },
	quickfile = { enabled = true },
	statuscolumn = { enabled = true },
	scroll = { enabled = true },
})

vim.keymap.set("n", "<leader><leader>", Snacks.picker.files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>pf", Snacks.picker.git_files, { desc = "Git Files" })
vim.keymap.set("n", "<leader>gl", Snacks.picker.git_log, { desc = "Git Log" })
vim.keymap.set("n", "<leader>pp", Snacks.picker.pick, { desc = "All Pickers" })
vim.keymap.set("n", "<leader>j", Snacks.picker.grep, { desc = "Grep" })
vim.keymap.set("n", "<leader>b", Snacks.picker.buffers, { desc = "Buffers" })
vim.keymap.set({ "n", "v" }, "<leader>J", Snacks.picker.grep_word, { desc = "Grep Word" })
vim.keymap.set({ "n", "i", "t" }, "<C-j>", Snacks.terminal.toggle, { desc = "Toggle Terminal" })

require("mason").setup({})
require("mason-lspconfig").setup({ ensure_installed = { "lua_ls", "gopls", "ts_ls" } })

vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		local client = vim.lsp.get_client_by_id(args.data.client_id)

		vim.lsp.inlay_hint.enable(true, { bufnr = args.buf })
		if client and client:supports_method("textDocument/completion") then
			vim.lsp.completion.enable(true, client.id, args.buf, {
				autotrigger = true,
			})
		end

		local opts = { buffer = args.buf, expr = true, replace_keycodes = false }
		vim.keymap.set("i", "<Tab>", function()
			if vim.fn.pumvisible() == 1 then
				return vim.keycode("<C-y>")
			else
				return vim.keycode("<Tab>")
			end
		end, opts)

		vim.keymap.set("i", "<CR>", function()
			if vim.fn.pumvisible() == 1 then
				return vim.keycode("<C-y>")
			else
				return vim.keycode("<CR>")
			end
		end, opts)

		vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf, desc = "Open Floating Diagnostic" })
		vim.keymap.set("n", "gd", Snacks.picker.lsp_definitions, { buffer = args.buf, desc = "[g]oto [d]efinition" })
		vim.keymap.set("n", "grr", Snacks.picker.lsp_references, { buffer = args.buf, desc = "[g]oto [r]eferences" })
		vim.keymap.set(
			"n",
			"gri",
			Snacks.picker.lsp_implementations,
			{ buffer = args.buf, desc = "[g]oto [i]mplmentations" }
		)
		vim.keymap.set("n", "gO", Snacks.picker.lsp_symbols, { buffer = args.buf, desc = "[g]oto symbol" })
		vim.keymap.set("n", "<leader>o", Snacks.picker.lsp_symbols, { buffer = args.buf, desc = "[g]oto symbol" })
		vim.keymap.set(
			"n",
			"<leader>O",
			Snacks.picker.lsp_workspace_symbols,
			{ buffer = args.buf, desc = "[g]oto workspace symbol" }
		)
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

require("conform").setup({ -- Autoformat on save
	formatters_by_ft = {
		php = nil,
		go = { "goimports" },
		lua = { "stylua" },
		json = { "jq" },
		-- javascript = { "eslint_d" },
		-- typescript = { "eslint_d" },
		-- javascriptreact = { "eslint_d" },
		-- typescriptreact = { "eslint_d" },
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

vim.api.nvim_create_autocmd("FileType", {
	callback = function(args)
		pcall(vim.treesitter.start, args.buf)
	end,
})

require("nvim-treesitter").install({
	"bash",
	"c",
	"cpp",
	"fish",
	"gitcommit",
	"go",
	"graphql",
	"html",
	"hyprlang",
	"java",
	"javascript",
	"json",
	"json5",
	"lua",
	"markdown",
	"markdown_inline",
	"python",
	"query",
	"rasi",
	"regex",
	"rust",
	"scss",
	"toml",
	"tsx",
	"typescript",
	"vim",
	"vimdoc",
	"yaml",
})

require("noice").setup({
	lsp = {
		progress = { enabled = false },
		override = {
			["vim.lsp.util.convert_input_to_markdown_lines"] = true,
			["vim.lsp.util.stylize_markdown"] = true,
		},
	},
	popupmenu = { enabled = false },
	presets = {
		bottom_search = true, -- use a classic bottom cmdline for search
		command_palette = true, -- position the cmdline and popupmenu together
		long_message_to_split = true, -- long messages will be sent to a split
		inc_rename = true, -- enables an input dialog for inc-rename.nvim
		lsp_doc_border = true, -- add a border to hover docs and signature help
	},
})

vim.o.rtp = vim.o.rtp .. vim.fn.expand(",~/dev/http.nvim")

require("http").setup({})
