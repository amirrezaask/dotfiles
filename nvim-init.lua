vim.g.mapleader = " "
vim.o.wrap = true
vim.o.breakindent = true
vim.o.signcolumn = 'yes'
vim.o.swapfile = false
vim.o.backup = false
vim.o.undofile = true
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.showmode = false
vim.o.sw = 4
vim.o.ts = 4
vim.o.expandtab = true
vim.o.timeoutlen = 300
vim.o.updatetime = 250
vim.o.number = true
vim.o.clipboard = "unnamedplus"
vim.o.ignorecase = true; vim.o.smartcase = true
vim.o.completeopt = "fuzzy,menu,noinsert,noselect,popup"
vim.o.statusline = "[%l:%c] %m%r%q%h%f"
vim.o.winborder = 'rounded'
vim.keymap.set("n", "Y", "^v$y", { desc = "Copy whole line" })
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
vim.keymap.set("n", "Q", "<cmd>q<CR>")
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
vim.keymap.set("n", "j", "gj"); vim.keymap.set("n", "k", "gk")
vim.keymap.set("t", "<C-w><C-w>", "<cmd>wincmd w<cr>")
vim.keymap.set("n", "<leader>i", ":edit $MYVIMRC<CR>")
vim.keymap.set("n", "<C-q>", function()
	local wins = vim.api.nvim_list_wins()
	local has_qf_open = false
	for _, win in ipairs(wins) do
		local buf = vim.api.nvim_win_get_buf(win)
		if vim.api.nvim_get_option_value("buftype", { buf = buf }) == "quickfix" then
			has_qf_open = true
		end
	end
	if has_qf_open then
		vim.cmd([[ cclose ]])
	else
		vim.cmd([[ copen ]])
	end
end, { desc = "Toggle Quickfix list" })
vim.keymap.set("n", "{", "<cmd>cprev<CR>")
vim.keymap.set("n", "}", "<cmd>cnext<CR>")

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
	if vim.v.shell_error ~= 0 then
		vim.api.nvim_echo({
			{ "Failed to clone lazy.nvim:\n", "ErrorMsg" },
			{ out, "WarningMsg" },
			{ "\nPress any key to exit..." },
		}, true, {})
		vim.fn.getchar()
		os.exit(1)
	end
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup {
    "folke/tokyonight.nvim",
    { "rose-pine/neovim", name = "rose-pine" },
	"neovim/nvim-lspconfig", "williamboman/mason.nvim" ,
    { "saghen/blink.cmp", version = "1.*",  },
    { "nvim-treesitter/nvim-treesitter" },
	{ "ibhagwan/fzf-lua" },
}

vim.cmd.colorscheme("rose-pine")

require("blink.cmp").setup  { keymap = { preset = "enter" }, cmdline = { enabled = false } }

require("nvim-treesitter.configs").setup { ensure_installed = { "lua", "go", "gomod", "php" }, highlight = { enable = true } }

local lspconfig = require("lspconfig")

for _, lsp in ipairs({ "gopls", "ols", "intelephense", "rust_analyzer", "zls" }) do lspconfig[lsp].setup {} end
lspconfig.lua_ls.setup({ settings = { Lua = { diagnostics = { globals = { "vim" } } } } })

vim.api.nvim_create_autocmd("LspAttach", { callback = function(args)
        local bufnr = args.buf
        local map = function(mode, key, fn, desc) vim.keymap.set(mode, key, fn, { buffer = bufnr, desc = "LSP: " .. desc }) end

        map("n", "[[", function() vim.diagnostic.jump({ count = -1 }) end, "Diagnostics: Next")
        map("n", "]]", function() vim.diagnostic.jump({ count = 1 }) end, "Diagnostics: Previous")
        map("n", "C-]", vim.lsp.buf.definition, "[g]oto definition")
        map("n", "gd", vim.lsp.buf.definition, "[g]oto [d]efinition")
        map("n", "gD", vim.lsp.buf.declaration, "[g]oto [D]eclaration")
        map("n", "gi", vim.lsp.buf.references, "[g]oto [i]mplementation")
        map("n", "gr", vim.lsp.buf.implementation, "[g]oto [r]eferences")
        map("n", "R", vim.lsp.buf.rename, "Rename")
        map("n", "K", vim.lsp.buf.hover, "Hover")
        map("n", "C", vim.lsp.buf.code_action, "Code Actions")
        map({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, "Signature Help")
        map("n", "<leader>l", vim.diagnostic.open_float, "Diagnostics: Open float window")
        map("n", "<leader>q", vim.diagnostic.setloclist, "Set Local list")
    end,
})

local fzflua = require "fzf-lua"
fzflua.setup { files = { previewer = false } }
vim.keymap.set("n", "<leader><leader>", fzflua.files, {})
vim.keymap.set("n", "<C-p>", fzflua.git_files, {})
vim.keymap.set("n", "<leader>fd", function() fzflua.files { cwd = "~/.dotfiles" } end, {})
vim.keymap.set("n", "??", fzflua.live_grep, {})
vim.keymap.set("n", "<leader>h", fzflua.helptags, {})
vim.keymap.set("n", "<leader>d", fzflua.diagnostics_document, {})
vim.keymap.set("n", "<leader>D", fzflua.diagnostics_workspace, {})
vim.keymap.set("n", "<leader>o", fzflua.lsp_document_symbols, {})
vim.keymap.set("n", "<leader>O", fzflua.lsp_workspace_symbols, {})

require("mason").setup({ ensure_installed = { "gopls" } })
vim.fn.setenv("PATH", os.getenv("PATH") .. ":" .. vim.fn.stdpath("data") .. "/mason/bin")
