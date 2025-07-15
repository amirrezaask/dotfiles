vim.g.mapleader = " " -- <leader> in keybindings means Space.
vim.o.wrap = true -- Wrap long lines.
vim.o.signcolumn = "no"
vim.o.breakindent = true -- Indent wrapped lines.
vim.o.swapfile = false -- Disable swapfile.
vim.o.undofile = true -- Store undo history on disk
vim.o.splitbelow = true -- Split windows below the current windows
vim.o.splitright = true -- Split windows right to the current windows
vim.o.clipboard = "unnamedplus" -- Copy/Cut/Paste to system clipboard
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.ignorecase = true -- Search case insensitive...
vim.o.smartcase = true -- ... but not if it contains caps
vim.o.formatoptions = "jcql" -- See :help fo-table
-- vim.o.updatetime = 100 -- Faster completion
vim.o.termguicolors = true -- Enable 24-bit RGB colors
vim.o.inccommand = "split" -- Show partial commands in the command line
vim.o.winborder = "rounded"
-- Ways to escape the INSERT mode
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
vim.keymap.set("i", "<C-c>", "<esc>")
-- Moving around will always keep you at the center.
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
-- Disable inc-search on <CR>
vim.keymap.set("n", "<CR>", "v:hlsearch ? ':nohlsearch<CR>' : '<CR>'", { expr = true, noremap = true })
-- Wrapped lines are just lines.
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
-- Quickfix list navigation.
vim.keymap.set("n", "{", "<cmd>cprev<CR>")
vim.keymap.set("n", "}", "<cmd>cnext<CR>")
-- Fat finger support
vim.cmd([[ command! W w ]])
vim.cmd([[ command! Q q ]])

-- Improve default colors
vim.cmd([[
	hi Normal guibg=none
	hi! link StatusLine  Normal
	hi! link NormalFloat Normal
	hi! Function guifg=NvimLightBlue
]])

local paq_installed, paq = pcall(require, "paq")
if not paq_installed then
	vim.fn.system(
		'git clone --depth=1 https://github.com/savq/paq-nvim.git "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/pack/paqs/start/paq-nvim'
	)
end

paq({
	"savq/paq-nvim",
	{ "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
	"ibhagwan/fzf-lua",
	"neovim/nvim-lspconfig",
	{ "saghen/blink.cmp", branch = "v1.1.1" },
	"stevearc/conform.nvim",
})

-- Fuzzy Finder
-- https://github.com/junegunn/fzf
Fzf = require("fzf-lua")
Fzf.setup({
	"fzf-vim", -- setup similar to fzf.vim
	keymap = { fzf = { ["ctrl-q"] = "select-all+accept" } },
})
Fzf.register_ui_select()
vim.keymap.set("n", "<leader><leader>", Fzf.files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>j", Fzf.lines, { desc = "Buffer Lines" })
vim.keymap.set("n", "<leader>l", Fzf.live_grep, { desc = "Live Grep" })
vim.keymap.set("n", "<leader>w", Fzf.grep, { desc = "Grep word" })
vim.keymap.set({ "n", "v" }, "<leader>.", Fzf.grep_cword, { desc = "Grep <cword>" })
vim.keymap.set("n", "<leader>s", Fzf.lsp_document_symbols, { desc = "LSP Document Symbols" })
vim.keymap.set("n", "<leader>S", Fzf.lsp_live_workspace_symbols, { desc = "LSP Workspace Symbols" })
vim.lsp.buf.definition = Fzf.lsp_definitions
vim.lsp.buf.implementation = Fzf.lsp_implementations
vim.lsp.buf.references = Fzf.lsp_references
vim.lsp.buf.type_definition = Fzf.lsp_type_definitions

-- Treesitter enables richer syntax highlight
require("nvim-treesitter.configs").setup({
	ensure_installed = { "go", "php" },
	auto_install = true,
	highlight = { enable = true },
	indent = { enable = true, disable = { "ruby" } },
})

-- Langauge Server Protocol:
-- github.com/LuaLS/lua-language-server/releases/latest
-- go install golang.org/x/tools/gopls@latest
-- npm install -g intelephense
for _, lsp in pairs({ "gopls", "intelephense" }) do
	require("lspconfig")[lsp].setup({})
end

require("lspconfig").lua_ls.setup({
	settings = {
		Lua = {
			diagnostics = { enable = true, globals = { "vim" } },
			workspace = { library = { vim.fn.expand("$VIMRUNTIME/lua") }, maxPreload = 10000, preloadFileSize = 10000 },
		},
	},
})

vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		vim.keymap.set("n", "[[", function()
			vim.diagnostic.jump({ count = -1 })
		end, { buffer = args.buf })
		vim.keymap.set("n", "]]", function()
			vim.diagnostic.jump({ count = 1 })
		end, { buffer = args.buf })
		vim.keymap.set("n", "C-]", vim.lsp.buf.definition, { buffer = args.buf })
		vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
		vim.keymap.set("n", "gr", vim.lsp.buf.references, { buffer = args.buf })
		vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { buffer = args.buf })
		vim.keymap.set("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
		vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
		vim.keymap.set("n", "L", vim.diagnostic.open_float, { buffer = args.buf })
		vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
		vim.keymap.set({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, { buffer = args.buf })
	end,
})
vim.diagnostic.config({ virtual_text = true })

-- Autocomplete
require("blink.cmp").setup({
	keymap = { preset = "enter" },
	cmdline = { enabled = false },
	completion = {
		list = { selection = { preselect = false } },
		menu = {
			draw = {
				columns = {
					{ "label", "label_description", gap = 1 },
				},
			},
		},
	},
	sources = {
		default = { "lsp", "path", "snippets" },
	},
})

-- Autoformat
-- github.com/JohnnyMorganz/StyLua/releases/latest
-- go install golang.org/x/tools/cmd/goimports@latest
require("conform").setup({
	formatters_by_ft = {
		lua = { "stylua" },
		go = { "goimports" },
	},
	format_on_save = {},
})
