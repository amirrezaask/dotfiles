vim.g.mapleader = " "
vim.g.maplocalleader = " "
K = vim.keymap.set

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

vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()" -- Set by treesitter plugin
vim.opt.foldcolumn = "0"
vim.opt.foldtext = ""
vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 5
vim.opt.foldnestmax = 4

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

local ok, ui2 = pcall(require, "vim._core.ui2") -- EXPERIMENTAL: Neovim 0.12 new UI
if ok then
	ui2.enable({ enable = true })
end

vim.api.nvim_create_autocmd("TextYankPost", { -- Highlight yanked text briefly
	callback = function()
		vim.hl.on_yank({ higroup = "Visual", timeout = 150 })
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

K("i", "jk", "<esc>")
K("i", "kj", "<esc>")
K("i", "<C-c>", "<esc>")

K("n", "<C-d>", "<C-d>zz")
K("n", "<C-u>", "<C-u>zz")

K("n", "n", "nzz")
K("n", "N", "Nzz")

K("n", "j", "gj")
K("n", "k", "gk")

K("n", "<leader>i", ":edit $MYVIMRC<CR>")

K("i", "<C-Space>", "<C-x><C-o>", { desc = "Trigger LSP completion" })
K("n", "<CR>", function() -- Clear search highlight with Enter
	if vim.v.hlsearch == 1 then
		vim.cmd.nohl()
		return ""
	else
		return vim.keycode("<CR>")
	end
end, { expr = true })

local gh = function(repo)
	return "https://github.com/" .. repo
end

vim.pack.add({
	gh("mason-org/mason.nvim"),
	gh("mason-org/mason-lspconfig.nvim"),
	gh("neovim/nvim-lspconfig"),
	gh("stevearc/conform.nvim"),
	gh("stevearc/oil.nvim"),
	gh("ibhagwan/fzf-lua"),
	gh("nvim-treesitter/nvim-treesitter"),
}, { confirm = false, load = true })

FzfLua = require("fzf-lua")
FzfLua.setup({ "fzf-vim" })
K("n", "<leader><leader>", FzfLua.files)
K("n", "<leader>pf", FzfLua.git_files)
K("n", "<leader>j", FzfLua.live_grep)
K({ "n", "v" }, "<leader>J", FzfLua.grep_cword)

require("mason").setup({})
require("mason-lspconfig").setup({ ensure_installed = { "lua_ls", "gopls" } })

vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		local client = vim.lsp.get_client_by_id(args.data.client_id)

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

		K("n", "L", vim.diagnostic.open_float, { buffer = args.buf })
		K("n", "gd", FzfLua.lsp_definitions)
		K("n", "grr", FzfLua.lsp_references)
		K("n", "gri", FzfLua.lsp_implementations)
		K("n", "gO", FzfLua.lsp_document_symbols)
		K("n", "<leader>o", FzfLua.lsp_document_symbols)
		K("n", "<leader>O", FzfLua.lsp_live_workspace_symbols)
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
