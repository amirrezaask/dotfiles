vim.g.mapleader = " "
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
vim.cmd([[ autocmd TextYankPost * silent! lua vim.hl.on_yank {higroup='Visual', timeout=150 } ]])
local map = vim.keymap.set

map("i", "jk", "<esc>")
map("i", "kj", "<esc>")
map("i", "<C-c>", "<esc>")
map("n", "<C-d>", "<C-d>zz")
map("n", "<C-u>", "<C-u>zz")
map("n", "n", "nzz")
map("n", "N", "Nzz")
map("n", "<leader>i", ":edit $MYVIMRC<CR>")

-- Bootstrap lazy.nvim
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

local plugins = {
	{
		'stevearc/conform.nvim',
		opts = {
			formatters_by_ft = {
				php = {},
				go = { "goimports" },
				lua = { "stylua" }
			},
			format_on_save = {
				timeout_ms = 500,
				lsp_format = "fallback"
			}
		},
	},
	{
		"mason-org/mason-lspconfig.nvim",
		opts = {
			ensure_installed = { "lua_ls", "gopls" }
		},
		dependencies = {
			{ "mason-org/mason.nvim", opts = {} },
			"neovim/nvim-lspconfig",
		},
	},
	{ "folke/snacks.nvim", opts = { dashboard = { enabled = true } } },
	{ "ibhagwan/fzf-lua",  opts = { "fzf-vim", keymap = { fzf = { ["ctrl-q"] = "select-all+accept" } } } },
	"nvim-tree/nvim-web-devicons",
	{ "nvim-treesitter/nvim-treesitter", main = "nvim-treesitter.configs", opts = { highlight = { enable = true }, auto_install = true } },
	{ "stevearc/oil.nvim",               opts = {} },
	"tpope/vim-surround",
	"tpope/vim-unimpaired",
	{ "saghen/blink.cmp",          version = "v1.6.0",                               opts = {} },
	{ 'nvim-lualine/lualine.nvim', dependencies = { 'nvim-tree/nvim-web-devicons' }, opts = {} }
}

local ok, theme_manager = pcall(require, "theme-manager")
if ok and theme_manager.lazy_spec then
	table.insert(plugins, theme_manager.lazy_spec)
end

if ok and theme_manager.callback then
	theme_manager.callback()
end

require("lazy").setup(plugins)

vim.cmd [[
	hi! Normal       guibg=none
	hi! SignColumn   guibg=none
	hi! StatusLine   guibg=none
	hi! NormalFloat  guibg=none

]]

vim.cmd([[
	nnoremap <leader><leader> <cmd>lua FzfLua.files()<CR>
	nnoremap <leader>j        <cmd>lua FzfLua.live_grep()<CR>
	nnoremap <leader>k        <cmd>lua FzfLua.grep_cword()<CR>
	vnoremap <leader>k        <cmd>lua FzfLua.grep_cword()<CR>
]])

vim.lsp.buf.references = FzfLua.lsp_references
vim.lsp.buf.definition = FzfLua.lsp_definitions
vim.lsp.buf.implementation = FzfLua.lsp_implementations
vim.lsp.buf.document_symbol = FzfLua.lsp_document_symbols
vim.lsp.buf.workspace_symbol = FzfLua.lsp_workspace_symbols

vim.diagnostic.config({ virtual_text = true })

-- Default Keybindings
-- see :h lsp-defaults
-- see :h vim.lsp.buf.tagfunc()
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		-- local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
		-- if client:supports_method('textDocument/completion') then
		--   vim.lsp.completion.enable(true, client.id, args.buf, {autotrigger = true})
		-- end
		map("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
		map("n", "L", vim.diagnostic.open_float, { buffer = args.buf })
		map("n", "<leader>O", FzfLua.lsp_workspace_symbols, { buffer = args.buf })
	end,
})
vim.lsp.config("lua_ls", { settings = { Lua = { workspace = { library = vim.api.nvim_get_runtime_file("", true) } } } })
