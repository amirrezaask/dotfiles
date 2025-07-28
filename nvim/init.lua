vim.g.mapleader = " "
vim.o.number = true
vim.o.relativenumber = true
vim.o.signcolumn = "yes"
vim.o.swapfile = false
vim.o.clipboard = "unnamedplus"
vim.o.tabstop = 4
vim.o.shiftwidth = 0
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.formatoptions = "jcql"
vim.o.inccommand = "split"
vim.o.winborder = "rounded"
vim.o.guicursor = ""
vim.cmd([[ " Colors
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
	"https://github.com/ibhagwan/fzf-lua",
	"https://github.com/nvim-treesitter/nvim-treesitter",
	"https://github.com/neovim/nvim-lspconfig",
	"https://github.com/stevearc/conform.nvim",
	"https://github.com/stevearc/oil.nvim",
	{ "https://github.com/saghen/blink.cmp", version = "1.6.0" },
}

require("conform").setup({ formatters_by_ft = { lua = { "stylua" }, go = { "goimports" } }, format_on_save = {} })
require("nvim-treesitter.configs").setup { ensure_installed = { "go", "php" }, highlight = { enable = true }, auto_install = true }
require("oil").setup()
require("blink.cmp").setup {
	keymap = { preset = "enter" },
	completion = { list = { selection = { preselect = false } }, menu = { draw = { columns = { { "label", "label_description", gap = 1 } } } } },
}

require("fzf-lua").setup { "fzf-vim", keymap = { fzf = { ["ctrl-q"] = "select-all+accept" } } }
require("fzf-lua").register_ui_select()

vim.keymap.set("n", "<leader><leader>", require("fzf-lua").files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>j", require("fzf-lua").live_grep, { desc = "Live Grep" })
vim.keymap.set({ "n", "v" }, "<leader>k", require("fzf-lua").grep_cword, { desc = "Grep <cword>" })

vim.lsp.buf.definition = require("fzf-lua").lsp_definitions
vim.lsp.buf.implementation = require("fzf-lua").lsp_implementations
vim.lsp.buf.references = require("fzf-lua").lsp_references
vim.lsp.buf.type_definition = require("fzf-lua").lsp_type_definitions

vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("n", "[[", function() vim.diagnostic.jump({ count = -1 }) end, {})
vim.keymap.set("n", "]]", function() vim.diagnostic.jump({ count = 1 }) end, {})

vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		vim.keymap.set("n", "C-]", vim.lsp.buf.definition, { buffer = args.buf })
		vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
		vim.keymap.set("n", "gr", vim.lsp.buf.references, { buffer = args.buf })
		vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { buffer = args.buf })
		vim.keymap.set("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
		vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
		vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
		vim.keymap.set({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, { buffer = args.buf })
		vim.keymap.set("n", "L", vim.diagnostic.open_float, {})
	end,
})

vim.lsp.enable({ "gopls", "intelephense", "lua_ls" })
