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
vim.o.linebreak = true
vim.o.completeopt = "menuone,noselect,noinsert,fuzzy"
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.laststatus = 3

vim.cmd [[
	set wildoptions+=fuzzy

	hi! Normal guibg=none
	hi! link StatusLine Normal

	autocmd TextYankPost * silent! lua vim.hl.on_yank {higroup='Visual', timeout=150 }

	imap jk    <ESC>
	imap kj    <ESC>
	imap <C-c> <ESC>

	nmap <C-d> <C-d>zz
	nmap <C-u> <C-u>zz
	nmap n     nzz
	nmap N     Nzz

	nmap <leader>i :edit $MYVIMRC<CR>
	nmap <leader>o :Oil<CR>
]]

vim.pack.add {
	{ src = "https://github.com/rose-pine/neovim", name = "rose-pine" },
	-- "https://github.com/vague2k/vague.nvim",

	"https://github.com/ibhagwan/fzf-lua",					               -- Fuzzy Finder
	"https://github.com/nvim-treesitter/nvim-treesitter",	               -- Syntax Highlighting
	"https://github.com/neovim/nvim-lspconfig",				               -- LSP
	"https://github.com/stevearc/oil.nvim",					               -- File manager
}

require("nvim-treesitter.configs").setup { highlight = { enable = true }, auto_install = true }
require("oil").setup {}
require("rose-pine").setup { disable_background = true, styles = { transparency = true } }
vim.cmd.colorscheme("rose-pine-moon")
-- require("vague").setup { transparent = true, italic = false }

require("fzf-lua").setup { "fzf-vim", keymap = { fzf = { ["ctrl-q"] = "select-all+accept" } } }

vim.cmd [[
	nnoremap <leader><leader> <cmd>lua FzfLua.files()<CR>
	nnoremap <leader>j        <cmd>lua FzfLua.live_grep()<CR>
	nnoremap <leader>k        <cmd>lua FzfLua.grep_cword()<CR>
	vnoremap <leader>k        <cmd>lua FzfLua.grep_cword()<CR>
]]

vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
		if client:supports_method('textDocument/completion') then
		  vim.lsp.completion.enable(true, client.id, args.buf, {autotrigger = true})
		end
		vim.cmd [[
			nnoremap <buffer> <C-]>          <cmd>lua vim.lsp.buf.definition() <CR>
			nnoremap <buffer> gd             <cmd>lua vim.lsp.buf.definition() <CR>
			nnoremap <buffer> gr             <cmd>lua FzfLua.lsp_references() <CR>
			nnoremap <buffer> gi             <cmd>lua FzfLua.lsp_implementations() <CR>
			nnoremap <buffer> R              <cmd>lua vim.lsp.buf.rename()<CR>
			nnoremap <buffer> K              <cmd>lua vim.lsp.buf.hover()<CR>
			nnoremap <buffer> C              <cmd>lua vim.lsp.buf.code_action()<CR>
			nnoremap <buffer> L              <cmd>lua vim.diagnostic.open_float()<CR>
		    nnoremap <buffer> <leader>l      <cmd>lua FzfLua.lsp_workspace_symbols()<CR>
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

vim.lsp.config('lua_ls', { settings = { Lua = { workspace = { library = vim.api.nvim_get_runtime_file('', true) } } } })

vim.lsp.enable({ "gopls", "intelephense", "lua_ls" })
