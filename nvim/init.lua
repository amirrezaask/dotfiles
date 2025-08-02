vim.g.mapleader = " "
vim.o.signcolumn = "yes"
vim.o.swapfile = false
vim.o.clipboard = "unnamedplus"
vim.o.tabstop = 4;
vim.o.shiftwidth = 0
vim.o.ignorecase = true;
vim.o.smartcase = true
vim.o.formatoptions = "jcql"
vim.o.inccommand = "split"
vim.o.winborder = "rounded"
vim.cmd [[ set completeopt=menuone,noselect,popup ]]
vim.cmd [[ hi! link StatusLine Normal ]]

vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
vim.keymap.set("i", "<C-c>", "<esc>")

vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")

vim.pack.add {
	"https://github.com/ibhagwan/fzf-lua", -- Fuzzy Finder
	"https://github.com/nvim-treesitter/nvim-treesitter", -- Syntax Highlighting
	"https://github.com/neovim/nvim-lspconfig", -- LSP
	"https://github.com/stevearc/conform.nvim", -- Autoformat
	"https://github.com/stevearc/oil.nvim", -- File manager
}

require("conform").setup({ formatters_by_ft = { go = { "goimports" } }, format_on_save = {} })
require("nvim-treesitter.configs").setup { highlight = { enable = true }, auto_install = true }
require("oil").setup()

local FzfLua = require("fzf-lua")
FzfLua.setup { "fzf-vim", keymap = { fzf = { ["ctrl-q"] = "select-all+accept" } } }
FzfLua.register_ui_select()

vim.keymap.set("n", "<leader><leader>", FzfLua.files)
vim.keymap.set("n", "<leader>j",        FzfLua.live_grep)
vim.keymap.set("n", "<leader>k",        FzfLua.grep_cword)
vim.keymap.set("v", "<leader>k",        FzfLua.grep_visual)

vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
        local client = vim.lsp.get_client_by_id(args.data.client_id)
		vim.lsp.completion.enable(true, client.id, bufnr, { autotrigger = true })
		vim.keymap.set("n", "C-]", vim.lsp.buf.definition, { buffer = args.buf })
		vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
		vim.keymap.set("n", "gr", vim.lsp.buf.references, { buffer = args.buf })
		vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { buffer = args.buf })
		vim.keymap.set("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
		vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
		vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
		vim.keymap.set({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, { buffer = args.buf })
		vim.keymap.set("n", "L", vim.diagnostic.open_float, {})
		vim.keymap.set("n", "<leader>l", FzfLua.lsp_workspace_symbols, {buffer = args.buf })
		vim.keymap.set("n", "<leader>o", FzfLua.diagnostics_document, {buffer = args.buf })
		vim.keymap.set("n", "<leader>O", FzfLua.diagnostics_workspace, {buffer = args.buf })
	end,
})

vim.lsp.enable({ "gopls", "intelephense", "lua_ls" })
