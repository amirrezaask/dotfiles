vim.g.mapleader = " "
vim.o.number = true
vim.o.relativenumber = true
vim.o.signcolumn = "no"
vim.o.swapfile = false
vim.o.clipboard = "unnamedplus"
vim.o.tabstop = 4
vim.o.shiftwidth = 0
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.formatoptions = "jcql"
vim.o.inccommand = "split"
vim.o.winborder = "rounded"
vim.o.timeoutlen = 500
vim.o.guicursor = ""
vim.cmd("set completeopt+=noselect")

vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("n", "<CR>", "v:hlsearch ? ':nohlsearch<CR>' : '<CR>'", { expr = true, noremap = true })
vim.keymap.set("n", "<leader>i", ":edit $MYVIMRC<CR>")
vim.keymap.set("n", "<leader>w", ":write <CR>")
vim.keymap.set("n", "[[", function() vim.diagnostic.jump({ count = -1 }) end, {})
vim.keymap.set("n", "]]", function() vim.diagnostic.jump({ count = 1 }) end, {})

vim.cmd([[
	hi Normal guibg=none
	hi! link StatusLine  Normal
	hi! link NormalFloat Normal
]])

local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system {
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	}
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup { -- When vim.pack becomes more stable I will move to native package management.
	"https://github.com/nvim-treesitter/nvim-treesitter",
	"https://github.com/ibhagwan/fzf-lua",
	"https://github.com/neovim/nvim-lspconfig",
	"https://github.com/stevearc/conform.nvim",
	"https://github.com/stevearc/oil.nvim",
}

require("conform").setup({ formatters_by_ft = { lua = { "stylua" }, go = { "goimports" } }, format_on_save = {} })
require("nvim-treesitter.configs").setup { ensure_installed = { "go", "php" }, highlight = { enable = true }, auto_install = true }
require("oil").setup()

Fzf = require("fzf-lua")
Fzf.setup({ "fzf-vim", keymap = { fzf = { ["ctrl-q"] = "select-all+accept" } } })
Fzf.register_ui_select()
vim.keymap.set("n", "<leader><leader>", Fzf.files)
vim.keymap.set("n", "<leader>j", Fzf.live_grep)
vim.keymap.set("n", "<leader>k", Fzf.grep)
vim.keymap.set("n", "<leader>K", Fzf.grep_cword)
vim.keymap.set("v", "<leader>K", Fzf.grep_visual)
vim.keymap.set("n", "<leader>l", Fzf.lsp_document_symbols)
vim.keymap.set("n", "<leader>L", Fzf.lsp_live_workspace_symbols)

vim.lsp.enable({ "gopls", "intelephense", "lua_ls" })
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		local client = vim.lsp.get_client_by_id(args.data.client_id)
		if client ~= nil and client:supports_method("textDocument/completion") then
			vim.lsp.completion.enable(true, client.id, args.buf, { autotrigger = true })
			vim.keymap.set("n", "C-]", vim.lsp.buf.definition, { buffer = args.buf })
			vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
			vim.keymap.set("n", "gr", vim.lsp.buf.references, { buffer = args.buf })
			vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { buffer = args.buf })
			vim.keymap.set("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
			vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
			vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
			vim.keymap.set({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, { buffer = args.buf })
			vim.keymap.set("n", "L", vim.diagnostic.open_float, {})
		end
	end,
})
