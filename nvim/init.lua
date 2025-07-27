vim.g.mapleader = " " -- <leader> in keybindings means Space.
vim.o.wrap = true -- Wrap long lines.
vim.o.signcolumn = "no" -- No need for my code jump around because some plugin wants to show a sign in the left.
vim.o.breakindent = true -- Indent wrapped lines.
vim.o.swapfile = false -- Disable swapfile.
vim.o.clipboard = "unnamedplus" -- Copy/Cut/Paste to system clipboard
vim.o.tabstop = 4 -- Default indent size
vim.o.shiftwidth = 4 -- Default indent size
vim.o.ignorecase = true -- Search case insensitive...
vim.o.smartcase = true -- ... but not if it contains caps
vim.o.formatoptions = "jcql" -- See :help fo-table
vim.o.inccommand = "split" -- Show partial commands in the command line
vim.o.winborder = "rounded" -- All created windows in neovim will follow this setting for borders.
vim.o.timeoutlen = 500 -- Time in milliseconds to wait for a mapped sequence to complete.
vim.o.guicursor = ""
vim.cmd("set completeopt+=noselect") -- No auto selection of completion candidates really annoying default.

vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("n", "<CR>", "v:hlsearch ? ':nohlsearch<CR>' : '<CR>'", { expr = true, noremap = true })
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("n", "{", "<cmd>cprev<CR>")
vim.keymap.set("n", "}", "<cmd>cnext<CR>")
vim.keymap.set("n", "<leader>i", ":edit $MYVIMRC<CR>")
vim.keymap.set("n", "<leader>w", ":write <CR>")
vim.keymap.set("n", "[[", function() vim.diagnostic.jump({ count = -1 }) end, {})
vim.keymap.set("n", "]]", function() vim.diagnostic.jump({ count = 1 }) end, {})
vim.keymap.set("n", "L", vim.diagnostic.open_float, {})

-- -- Colors
vim.cmd([[
	hi Normal guibg=none
	hi! link StatusLine  Normal
	hi! link NormalFloat Normal
]])

vim.pack.add({
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter" },
	{ src = "https://github.com/ibhagwan/fzf-lua" },
	{ src = "https://github.com/neovim/nvim-lspconfig" },
	{ src = "https://github.com/stevearc/conform.nvim" },
	{ src = "https://github.com/stevearc/oil.nvim" },
	{ src = "https://github.com/vague2k/vague.nvim" },
})

require("conform").setup({ formatters_by_ft = { lua = { "stylua" }, go = { "goimports" } }, format_on_save = {} })
require("nvim-treesitter.configs").setup { ensure_installed = { "go", "php" }, highlight = { enable = true }, auto_install = true }
require("oil").setup()

Fzf = require("fzf-lua")
Fzf.setup({ "fzf-vim", keymap = { fzf = { ["ctrl-q"] = "select-all+accept" } } })
Fzf.register_ui_select()
vim.keymap.set("n", "<leader><leader>", Fzf.files, { desc = "Find Files" })
vim.keymap.set("n", "<leader>j", Fzf.live_grep, { desc = "Live Grep" })
vim.keymap.set("n", "<leader>k", Fzf.grep, { desc = "Grep word" })
vim.keymap.set("n", "<leader>K", Fzf.grep_cword, { desc = "Grep <cword>" })
vim.keymap.set("v", "<leader>K", Fzf.grep_visual, { desc = "Grep visualy selected text" })
vim.keymap.set("n", "<leader>l", Fzf.lsp_document_symbols, { desc = "LSP Document Symbols" })
vim.keymap.set("n", "<leader>L", Fzf.lsp_live_workspace_symbols, { desc = "LSP Workspace Symbols" })

vim.lsp.enable({ "gopls", "intelephense", "lua_ls" })
vim.api.nvim_create_autocmd("LspAttach", { -- Lsp keybindings
	callback = function(args)
		local client = vim.lsp.get_client_by_id(args.data.client_id)
		if client ~= nil and client:supports_method("textDocument/completion") then
			vim.lsp.completion.enable(true, client.id, args.buf, { autotrigger = true })
		end
		vim.keymap.set("n", "C-]", vim.lsp.buf.definition, { buffer = args.buf }) -- extend default vim
		vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
		vim.keymap.set("n", "gr", vim.lsp.buf.references, { buffer = args.buf })
		vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { buffer = args.buf })
		vim.keymap.set("n", "<C-j>", vim.lsp.buf.definition, { buffer = args.buf })
		vim.keymap.set("n", "<C-k>", vim.lsp.buf.references, { buffer = args.buf })
		vim.keymap.set("n", "<C-l>", vim.lsp.buf.implementation, { buffer = args.buf })
		vim.keymap.set("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
		vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
		vim.keymap.set("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
		vim.keymap.set({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, { buffer = args.buf })
	end,
})
