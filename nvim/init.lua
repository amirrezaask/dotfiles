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
vim.opt.wildoptions:append("fuzzy")
vim.o.cursorline = true
vim.cmd [[ autocmd TextYankPost * silent! lua vim.hl.on_yank {higroup='Visual', timeout=150 } ]]
local map = vim.keymap.set

map("i", "jk", "<esc>")
map("i", "kj", "<esc>")
map("i", "<C-c>", "<esc>")
map("n", "<C-d>", "<C-d>zz")
map("n", "<C-u>", "<C-u>zz")
map("n", "n", "nzz")
map("n", "N", "Nzz")
map("n", "<leader>i", ":edit $MYVIMRC<CR>")

vim.pack.add {
	"https://github.com/ibhagwan/fzf-lua",
	"https://github.com/scottmckendry/cyberdream.nvim",
	"https://github.com/folke/tokyonight.nvim",
	"https://github.com/nvim-tree/nvim-web-devicons",
	"https://github.com/nvim-treesitter/nvim-treesitter",
	"https://github.com/neovim/nvim-lspconfig",
	"https://github.com/stevearc/oil.nvim",
	{ src = 'https://github.com/saghen/blink.cmp', version="v1.6.0" }
}

vim.cmd [[ hi StatusLine guibg=none guifg=fg ]]

require("nvim-treesitter.configs").setup { highlight = { enable = true }, auto_install = true }

require("oil").setup {}

require("blink.cmp").setup {
    keymap = { preset = 'default' },
}
require("fzf-lua").setup { "fzf-vim", keymap = { fzf = { ["ctrl-q"] = "select-all+accept" } } }
vim.cmd [[
	nnoremap <leader><leader> <cmd>lua FzfLua.files()<CR>
	nnoremap <leader>j        <cmd>lua FzfLua.live_grep()<CR>
	nnoremap <leader>k        <cmd>lua FzfLua.grep_cword()<CR>
	vnoremap <leader>k        <cmd>lua FzfLua.grep_cword()<CR>
]]

vim.lsp.buf.references       = FzfLua.lsp_references
vim.lsp.buf.definition       = FzfLua.lsp_definitions
vim.lsp.buf.implementation   = FzfLua.lsp_implementations
vim.lsp.buf.document_symbol  = FzfLua.lsp_document_symbols
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
		map("n", "gd", vim.lsp.buf.definition, {buffer = args.buf})
		map("n", "L", vim.diagnostic.open_float, {buffer = args.buf})
		map("n", "<leader>O", FzfLua.lsp_workspace_symbols, {buffer = args.buf})
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
