-- Use Space as the global leader and localleader so custom mappings are easy to type.
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- -----------------------------------------------------------------------------
-- Core editor behavior
-- -----------------------------------------------------------------------------

vim.o.undofile = true -- Persist undo history across editor restarts.
vim.o.swapfile = false -- Avoid creating swap files next to edited files.
vim.o.number = true -- Show absolute line numbers.
vim.o.relativenumber = true -- Show relative line numbers for faster motions.
vim.o.signcolumn = "yes" -- Keep the sign column visible to avoid text shifting.
vim.o.cursorline = true -- Highlight the line under the cursor.
vim.o.scrolloff = 5 -- Keep five lines of context above/below the cursor.
vim.o.linebreak = true -- Wrap long lines at word boundaries instead of mid-word.
vim.o.winborder = "rounded" -- Use rounded borders for floating windows.
vim.o.laststatus = 3 -- Use one global statusline instead of one per window.

-- -----------------------------------------------------------------------------
-- Folding
-- -----------------------------------------------------------------------------

vim.o.foldmethod = "expr" -- Compute folds from an expression instead of markers/indent.
vim.o.foldexpr = "v:lua.vim.treesitter.foldexpr()" -- Let Tree-sitter provide semantic folds.
vim.o.foldcolumn = "0" -- Hide the fold column in the gutter.
vim.o.foldtext = "" -- Use the default line text for folded regions.
vim.o.foldlevel = 99 -- Keep almost all folds open after changing buffers.
vim.o.foldlevelstart = 5 -- Start with moderately nested folds open when reading files.
vim.o.foldnestmax = 4 -- Cap fold nesting so deeply nested code stays navigable.

-- vim.o.winbar = "%f %l:%c" -- Optional per-window filename and cursor position.
vim.o.laststatus = 3 -- Re-assert the global statusline setting.

-- -----------------------------------------------------------------------------
-- Indentation, searching, completion, windows, and clipboard
-- -----------------------------------------------------------------------------

vim.o.tabstop = 4 -- Display a tab character as four spaces wide.
vim.o.shiftwidth = 0 -- Use 'tabstop' as the indent width.

vim.o.ignorecase = true -- Search case-insensitively by default.
vim.o.smartcase = true -- Switch to case-sensitive search when the pattern has capitals.

vim.o.formatoptions = "jcql" -- Control automatic comment/text formatting behavior.
vim.o.inccommand = "split" -- Preview substitutions in a live split window.
vim.o.completeopt = "menuone,noselect,noinsert,fuzzy" -- Show completion menu without auto-inserting.

vim.o.splitbelow = true -- Open horizontal splits below the current window.
vim.o.splitright = true -- Open vertical splits to the right of the current window.
vim.o.splitkeep = "topline" -- Preserve the top visible line when opening splits.

vim.o.clipboard = "unnamedplus" -- Sync yank/delete/put with the system clipboard.

vim.opt.wildoptions:append("fuzzy") -- Enable fuzzy matching for command-line completion.

vim.diagnostic.config({ virtual_text = false }) -- Prefer diagnostic floats over inline virtual text.

-- vim.o.autocomplete = true -- Enable built-in automatic completion. See :h 'autocomplete', disabled for now since it collides with the lsp autocomplete.
vim.o.pumheight = 10 -- Limit completion popup height.
vim.o.pumblend = 10 -- Make the completion popup slightly transparent.

-- EXPERIMENTAL: Enable Neovim's new core UI layer when it exists.
local ok, ui2 = pcall(require, "vim._core.ui2")
if ok then
	ui2.enable({ enable = true })
end

-- -----------------------------------------------------------------------------
-- Autocommands
-- -----------------------------------------------------------------------------

vim.api.nvim_create_autocmd("TextYankPost", {
	-- Flash the yanked region so it is obvious what was copied.
	callback = function()
		vim.hl.on_yank({ higroup = "Visual", timeout = 150 })
	end,
})

vim.api.nvim_create_autocmd("BufEnter", {
	-- Prompt buffers do their own input handling, so autocomplete gets in the way.
	callback = function(args)
		if vim.bo[args.buf].buftype == "prompt" then
			vim.bo[args.buf].autocomplete = false
		end
	end,
})

vim.api.nvim_create_autocmd("VimResized", { command = "wincmd =" }) -- Equalize split sizes after terminal/window resize.

vim.api.nvim_create_autocmd("BufReadPost", {
	-- Restore the cursor to the last saved position when reopening a normal file.
	callback = function(args)
		local mark = vim.api.nvim_buf_get_mark(args.buf, '"')
		local line_count = vim.api.nvim_buf_line_count(args.buf)
		if mark[1] > 0 and mark[1] <= line_count then
			vim.api.nvim_win_set_cursor(0, mark)
			vim.schedule(function()
				vim.cmd("normal! zz") -- Center the restored cursor after the window settles.
			end)
		end
	end,
})

-- -----------------------------------------------------------------------------
-- Keymaps
-- -----------------------------------------------------------------------------

vim.keymap.set("i", "jk", "<esc>") -- Leave insert mode without reaching for Escape.
vim.keymap.set("i", "kj", "<esc>") -- Alternate insert-mode Escape chord.
vim.keymap.set("i", "<C-c>", "<esc>") -- Make Ctrl-C behave like Escape in insert mode.

vim.keymap.set("n", "<C-d>", "<C-d>zz") -- Half-page down and recenter.
vim.keymap.set("n", "<C-u>", "<C-u>zz") -- Half-page up and recenter.

vim.keymap.set("n", "n", "nzz") -- Next search result and recenter.
vim.keymap.set("n", "N", "Nzz") -- Previous search result and recenter.

vim.keymap.set("n", "j", "gj") -- Move by visual lines when text wraps.
vim.keymap.set("n", "k", "gk") -- Move by visual lines when text wraps.

vim.keymap.set("n", "<leader>i", ":edit $MYVIMRC<CR>", { desc = "Edit Configuration" })

vim.keymap.set("i", "<C-Space>", "<C-x><C-o>", { desc = "Trigger LSP completion" })
vim.keymap.set("n", "<CR>", function()
	-- Pressing Enter clears an active search highlight; otherwise it behaves normally.
	if vim.v.hlsearch == 1 then
		vim.cmd.nohl()
		return ""
	else
		return vim.keycode("<CR>")
	end
end, { expr = true })

-- -----------------------------------------------------------------------------
-- Plugin installation
-- -----------------------------------------------------------------------------

vim.pack.add({
	-- Themes kept installed so colorschemes can be swapped quickly.
	"https://github.com/folke/tokyonight.nvim",
	"https://github.com/vague-theme/vague.nvim",
	{ src = "https://github.com/catppuccin/nvim", name = "catppuccin" },
	{ src = "https://github.com/embark-theme/vim", name = "embark" },
	{ src = "https://github.com/rose-pine/neovim", name = "rose-pine" },
	"https://github.com/sainnhe/everforest",
	"https://github.com/ellisonleao/gruvbox.nvim",

	-- LSP server installation and Neovim LSP configuration helpers.
	"https://github.com/mason-org/mason.nvim",
	"https://github.com/mason-org/mason-lspconfig.nvim",
	"https://github.com/neovim/nvim-lspconfig",

	"https://github.com/stevearc/conform.nvim", -- Formatter runner and format-on-save integration.

	"https://github.com/stevearc/oil.nvim", -- Edit directories as buffers.

	"https://github.com/nvim-treesitter/nvim-treesitter", -- Tree-sitter parser management and highlighting.

	"https://github.com/folke/snacks.nvim", -- Collection of quality-of-life tools, especially pickers.

	-- UI framework and command/message replacement.
	"https://github.com/MunifTanjim/nui.nvim",
	"https://github.com/folke/noice.nvim",

	"https://github.com/folke/which-key.nvim", -- Show available keybindings as you type.

	"https://github.com/nvim-mini/mini.nvim",

	"https://github.com/lewis6991/gitsigns.nvim",

	"https://github.com/folke/trouble.nvim",
}, { confirm = false, load = true })

-- -----------------------------------------------------------------------------
-- Theme setup
-- -----------------------------------------------------------------------------

require("vague").setup({
	transparent = false, -- Keep an explicit background color.
	bold = false, -- Disable bold globally for a flatter look.
	italic = false, -- Disable italic globally for consistent text rendering.
})

vim.g.everforest_background = "hard" -- Use Everforest's highest-contrast dark variant.
vim.api.nvim_create_autocmd("ColorScheme", {
	pattern = "everforest",
	callback = function()
		-- Make key backgrounds match when Everforest is active.
		vim.cmd([[
			hi! Normal guibg=#1e2326
			hi! NormalFloat guibg=#1e2326
			hi! Terminal guibg=#1e2326
		]])
	end,
})

require("gruvbox").setup({
	undercurl = false, -- Avoid curly underline decorations.
	underline = false, -- Avoid underline decorations.
	bold = false, -- Keep the theme from applying bold text.
	italic = {
		strings = false,
		emphasis = false,
		comments = false,
		operators = false,
		folds = false,
	},
	contrast = "hard", -- Use the highest-contrast Gruvbox palette.
})

require("tokyonight").setup({
	transparent = true,
	styles = {
		comments = { italic = false }, -- Keep comments upright.
		keywords = { italic = false }, -- Keep keywords upright.
	},
})

vim.cmd.colorscheme("default") -- Active colorscheme.

if vim.g.colors_name then
	vim.api.nvim_set_hl(0, "SnacksPickerDir", { link = "SnacksPickerNormal" })
end

-- -----------------------------------------------------------------------------
-- Plugin configuration
-- -----------------------------------------------------------------------------

require("oil").setup({}) -- Use plugin defaults for directory editing.

require("mini.statusline").setup()
require("mini.icons").setup()

require("gitsigns").setup({})

require("which-key").setup({
	preset = "helix", -- Use Helix-style which-key layout.
	loop = true, -- Keep which-key open for repeated key exploration.
})

require("snacks").setup({
	bigfile = { enabled = true }, -- Disable expensive features for very large files.
	indent = { enabled = true }, -- Draw indentation guides.
	input = { enabled = true }, -- Use Snacks input UI.
	picker = { enabled = true }, -- Enable fuzzy pickers.
	notifier = { enabled = true }, -- Enable notification UI.
	quickfile = { enabled = true }, -- Speed up opening files passed on the command line.
	statuscolumn = { enabled = true }, -- Enhanced status column integration.
})

-- Snacks picker and terminal shortcuts.
vim.keymap.set("n", "<leader><leader>", Snacks.picker.files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>i", function()
	Snacks.picker.files({ cwd = "~/dev/dotfiles" })
end, { desc = "Find Configuration" })
vim.keymap.set("n", "<leader>pf", Snacks.picker.git_files, { desc = "Git Files" })
vim.keymap.set("n", "<leader>gl", Snacks.picker.git_log, { desc = "Git Log" })
vim.keymap.set("n", "<leader>pp", Snacks.picker.pick, { desc = "All Pickers" })
vim.keymap.set("n", "<leader>j", Snacks.picker.grep, { desc = "Grep" })
vim.keymap.set("n", "<leader>k", Snacks.picker.buffers, { desc = "Buffers" })
vim.keymap.set({ "n", "v" }, "<leader>J", Snacks.picker.grep_word, { desc = "Grep Word" })
vim.keymap.set({ "n", "i", "t" }, "<C-j>", Snacks.terminal.toggle, { desc = "Toggle Terminal" })

-- -----------------------------------------------------------------------------
-- LSP setup
-- -----------------------------------------------------------------------------

require("mason").setup({}) -- Install and manage external language tools.
require("mason-lspconfig").setup({ ensure_installed = { "lua_ls", "gopls", "ts_ls" } }) -- Keep core LSPs installed.

require("trouble").setup({})

vim.api.nvim_create_autocmd("LspAttach", {
	-- Configure buffer-local LSP behavior after a language server attaches.
	callback = function(args)
		local client = vim.lsp.get_client_by_id(args.data.client_id)

		vim.lsp.inlay_hint.enable(true, { bufnr = args.buf }) -- Show inline type/parameter hints.
		if client and client:supports_method("textDocument/completion") then
			-- Use Neovim's built-in LSP completion with automatic popup triggering.
			vim.lsp.completion.enable(true, client.id, args.buf, {
				autotrigger = true,
			})
		end

		local opts = { buffer = args.buf, expr = true, replace_keycodes = false }
		vim.keymap.set("i", "<Tab>", function()
			-- Accept the selected completion item when the popup menu is visible.
			if vim.fn.pumvisible() == 1 then
				return vim.keycode("<C-y>")
			else
				return vim.keycode("<Tab>")
			end
		end, opts)

		vim.keymap.set("i", "<CR>", function()
			-- Enter confirms completion when the popup is visible; otherwise it inserts a newline.
			if vim.fn.pumvisible() == 1 then
				return vim.keycode("<C-y>")
			else
				return vim.keycode("<CR>")
			end
		end, opts)

		-- LSP navigation uses Snacks pickers for previewable result lists.
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
				globals = { "vim" }, -- Teach lua_ls that the Neovim global exists.
			},
			workspace = { library = vim.api.nvim_get_runtime_file("", true) }, -- Add runtime files for better completion.
		},
	},
})

-- -----------------------------------------------------------------------------
-- Formatting
-- -----------------------------------------------------------------------------

require("conform").setup({
	formatters_by_ft = {
		php = nil, -- Explicitly skip PHP formatter configuration.
		go = { "goimports" }, -- Format Go and organize imports.
		lua = { "stylua" }, -- Format Lua with Stylua.
		json = { "jq" }, -- Format JSON with jq.
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
			-- For these filetypes, only run explicitly configured formatters.
			return { timeout_ms = 500, lsp_fallback = false }
		end
		-- Other filetypes may fall back to LSP formatting when no formatter is configured.
		return { timeout_ms = 500, lsp_fallback = true }
	end,
})

-- -----------------------------------------------------------------------------
-- Tree-sitter
-- -----------------------------------------------------------------------------

vim.api.nvim_create_autocmd("FileType", {
	-- Start Tree-sitter highlighting opportunistically; pcall avoids errors for missing parsers.
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

-- -----------------------------------------------------------------------------
-- Noice UI
-- -----------------------------------------------------------------------------

require("noice").setup({
	lsp = {
		progress = { enabled = false }, -- Hide noisy LSP progress messages.
		override = {
			["vim.lsp.util.convert_input_to_markdown_lines"] = true, -- Render LSP markdown through Noice.
			["vim.lsp.util.stylize_markdown"] = true, -- Improve markdown styling in LSP popups.
		},
	},
	popupmenu = { enabled = false }, -- Let built-in completion/Snacks handle popup menus.
	presets = {
		bottom_search = true, -- Use a classic bottom command line for search.
		command_palette = true, -- Position the command line and popup menu together.
		long_message_to_split = true, -- Send long messages to a split.
		inc_rename = true, -- Enable an input dialog for inc-rename.nvim.
		lsp_doc_border = true, -- Add a border to hover docs and signature help.
	},
})

-- -----------------------------------------------------------------------------
-- Local development plugins
-- -----------------------------------------------------------------------------

vim.o.rtp = vim.o.rtp .. vim.fn.expand(",~/dev/http.nvim") -- Add local http.nvim checkout to runtimepath.
require("http").setup({}) -- Configure the local http.nvim plugin with defaults.
