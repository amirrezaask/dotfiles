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
	"https://github.com/folke/snacks.nvim",                                -- Blazing fast fuzzy finder
	"https://github.com/nvim-treesitter/nvim-treesitter",	               -- Syntax Highlighting
	"https://github.com/neovim/nvim-lspconfig",				               -- LSP
	"https://github.com/stevearc/oil.nvim",					               -- File manager
}

require("nvim-treesitter.configs").setup { highlight = { enable = true }, auto_install = true }

require("oil").setup {}


require("snacks").setup { picker = { enabled = true }, terminal = { enabled = true } }
vim.cmd [[ hi! link SnacksPickerDir Comment ]]

vim.cmd [[
	nnoremap <leader><leader> <cmd>lua Snacks.picker.smart()<CR>
	nnoremap <leader>j        <cmd>lua Snacks.picker.grep()<CR>
	nnoremap <leader>k        <cmd>lua Snacks.picker.grep_word()<CR>
	vnoremap <leader>k        <cmd>lua Snacks.picker.grep_word()<CR>
	nnoremap <C-;>            <cmd>lua Snacks.terminal.toggle()<CR>
	tnoremap <C-;>            <cmd>lua Snacks.terminal.toggle()<CR>
]]

vim.lsp.buf.references       = Snacks.picker.lsp_references
vim.lsp.buf.definition       = Snacks.picker.lsp_definitions
vim.lsp.buf.implementation   = Snacks.picker.lsp_implementations
vim.lsp.buf.document_symbol  = Snacks.picker.lsp_symbols
vim.lsp.buf.workspace_symbol = Snacks.picker.lsp_workspace_symbols

vim.diagnostic.config({ virtual_text = true })

-- Default Keybindings
-- see :h lsp-defaults
-- see :h vim.lsp.buf.tagfunc()
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
		if client:supports_method('textDocument/completion') then
		  vim.lsp.completion.enable(true, client.id, args.buf, {autotrigger = true})
		end
		vim.cmd [[
			nnoremap <buffer> gd             <cmd>lua vim.lsp.buf.definition()<CR>
			nnoremap <buffer> L              <cmd>lua vim.diagnostic.open_float()<CR>
			nnoremap <buffer> <leader>O      <cmd>lua Snacks.picker.lsp_workspace_symbols()<CR>
		]]
	end
})
vim.lsp.config('lua_ls', { settings = { Lua = { workspace = { library = vim.api.nvim_get_runtime_file('', true) } } } })
vim.lsp.enable({ "gopls", "intelephense", "lua_ls" })

vim.api.nvim_create_autocmd('BufEnter', { -- Go related stuff
	pattern = "*.go",
	callback = function(args)
		vim.o.makeprg = "golangci-lint run --tests=false --show-stats=false"
		vim.o.errorformat = "%f:%l:%c %m"
		vim.api.nvim_create_autocmd("BufWritePre", { -- Autoformat + imports
			buffer = args.buf,
			callback = function()
				vim.lsp.buf.code_action({ context = { only = { 'source.organizeImports' }, }, apply = true, })
				vim.lsp.buf.format()
			end,
		})
	end
})

