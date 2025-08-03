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
vim.o.splitkeep = 'topline'
vim.o.showmode = false
vim.o.linebreak = true
vim.o.completeopt = "menuone,noselect,noinsert,fuzzy"
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.laststatus = 3

vim.cmd [[
	set wildoptions+=fuzzy

	hi! link StatusLine Normal
	hi! Visual guibg=fg guifg=bg

	autocmd TextYankPost * silent! lua vim.hl.on_yank {higroup='Visual', timeout=150 }

	imap jk    <ESC>
	imap kj    <ESC>
	imap <C-c> <ESC>

	nmap <C-d> <C-d>zz
	nmap <C-u> <C-u>zz
	nmap n     nzz
	nmap N     Nzz

	nmap <leader>i :edit $MYVIMRC<CR>
]]

vim.pack.add {
	"https://github.com/ibhagwan/fzf-lua",                -- Fuzzy Finder
	"https://github.com/nvim-treesitter/nvim-treesitter", -- Syntax Highlighting
	"https://github.com/neovim/nvim-lspconfig",           -- LSP
	"https://github.com/stevearc/oil.nvim",               -- File manager
}

require("nvim-treesitter.configs").setup { highlight = { enable = true }, auto_install = true }
require("oil").setup()

F = require("fzf-lua")
F.setup { "fzf-vim", keymap = { fzf = { ["ctrl-q"] = "select-all+accept" } } }
F.register_ui_select()

vim.cmd [[
	nnoremap <leader><leader> <cmd>lua F.files()<CR>
	nnoremap <leader>j        <cmd>lua F.live_grep()<CR>
	nnoremap <leader>k        <cmd>lua F.grep_cword()<CR>
	vnoremap <leader>k        <cmd>lua F.grep_cword()<CR>
]]

vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		local client = vim.lsp.get_client_by_id(args.data.client_id)
		vim.lsp.completion.enable(true, client.id, args.buf, { autotrigger = true })
		vim.cmd [[
			nnoremap <buffer> <C-]>          <cmd>lua vim.lsp.buf.definition() <CR>
			nnoremap <buffer> gd             <cmd>lua vim.lsp.buf.definition() <CR>
			nnoremap <buffer> gr             <cmd>lua F.lsp_references() <CR>
			nnoremap <buffer> gi             <cmd>lua F.lsp_implementations() <CR>
			nnoremap <buffer> R              <cmd>lua vim.lsp.buf.rename()<CR>
			nnoremap <buffer> K              <cmd>lua vim.lsp.buf.hover()<CR>
			nnoremap <buffer> C              <cmd>lua vim.lsp.buf.code_action()<CR>
			nnoremap <buffer> L              <cmd>lua vim.diagnostic.open_float()<CR>
		    nnoremap <buffer> <leader>l      <cmd>lua F.lsp_workspace_symbols()<CR>
			nnoremap <buffer> <C-s>          <cmd>lua vim.lsp.buf.signature_help()<CR>
			inoremap <buffer> <C-s>          <cmd>lua vim.lsp.buf.signature_help()<CR>
		]]
	end
})

-- Go Autoformat
vim.api.nvim_create_autocmd('BufWritePre', {
	pattern  = '*.go',
	callback = function()
		vim.lsp.buf.code_action({ context = { only = { 'source.organizeImports' }, }, apply = true, })
		vim.lsp.buf.format()
	end,
})

vim.lsp.config('lua_ls',
	{ settings = { Lua = { diagnostics = { globals = { 'vim' } }, workspace = { library = vim.api.nvim_get_runtime_file('', true) } } } })

vim.lsp.enable({ "gopls", "intelephense", "lua_ls" })
