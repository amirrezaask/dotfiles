vim.g.mapleader = " "
vim.o.signcolumn = "yes"
vim.o.swapfile = false
vim.o.clipboard = "unnamedplus"
vim.o.tabstop = 4; vim.o.shiftwidth = 0
vim.o.ignorecase = true; vim.o.smartcase = true
vim.o.formatoptions = "jcql"
vim.o.inccommand = "split"
vim.o.winborder = "rounded"
vim.cmd [[ set completeopt=menuone,noselect,popup ]]

vim.cmd([[ " Colors
	hi Normal guibg=none
	hi! link StatusLine  Normal
	hi! link NormalFloat Normal
]])

vim.pack.add {
	"https://github.com/ibhagwan/fzf-lua",
	"https://github.com/nvim-treesitter/nvim-treesitter",
	"https://github.com/neovim/nvim-lspconfig",
	"https://github.com/stevearc/conform.nvim",
	"https://github.com/stevearc/oil.nvim",
}

require("conform").setup({ formatters_by_ft = { go = { "goimports" } }, format_on_save = {} })
require("nvim-treesitter.configs").setup { highlight = { enable = true }, auto_install = true }
require("oil").setup()

require("fzf-lua").setup { "fzf-vim", keymap = { fzf = { ["ctrl-q"] = "select-all+accept" } } }; require("fzf-lua").register_ui_select()
vim.keymap.set("n", "<leader><leader>", require("fzf-lua").files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>j", require("fzf-lua").live_grep, { desc = "Live Grep" })
vim.keymap.set({ "n", "v" }, "<leader>k", require("fzf-lua").grep_cword, { desc = "Grep <cword>" })

vim.keymap.set("i", "jk", "<ESC>"); vim.keymap.set("i", "kj", "<ESC>"); vim.keymap.set("i", "<C-c>", "<esc>")

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
	end,
})

vim.lsp.enable({ "gopls", "intelephense", "lua_ls" })
