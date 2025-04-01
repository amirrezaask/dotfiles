local paq_install_path = vim.fn.stdpath("data") .. "/site/pack/paqs/start/paq-nvim"

if vim.fn.empty(vim.fn.glob(paq_install_path)) > 0 then -- Installing nvim-paq package manager if not installed
	print("Installing paq-nvim...")
	vim.fn.system({ "git", "clone", "--depth=1", "https://github.com/savq/paq-nvim.git", paq_install_path })
	print("paq-nvim installed! Restart Neovim and run :PaqInstall")
end

require("paq")({
	"ibhagwan/fzf-lua",
	"stevearc/conform.nvim",
	"neovim/nvim-lspconfig",
	"williamboman/mason.nvim",
	"supermaven-inc/supermaven-nvim",
	"nvim-treesitter/nvim-treesitter",
	"tpope/vim-fugitive",
	"folke/ts-comments.nvim",
	{ "rose-pine/neovim", as = "rose-pine" },
	{ "catppuccin/nvim", as = "catppuccin" },
	"amirrezaask/sitruuna.nvim",
})

local TRANSPARENT = true
-- colors
require("rose-pine").setup({ dark_variant = "moon", styles = { italic = false, transparency = TRANSPARENT } })
require("catppuccin").setup({ transparent_background = TRANSPARENT })

vim.cmd.colorscheme("catppuccin")
vim.cmd([[
    hi Normal guibg=none
    hi NormalFloat guibg=none
    hi StatusLine guibg=none
    hi LineNr guibg=none
    hi SignColumn guibg=none
]])

vim.g.mapleader = " " -- <leader> key for keymaps mapped to <Space>
vim.opt.wrap = true -- Wrap long lines
vim.opt.breakindent = true -- Wrapped lines have same indentation as the actual line.
vim.opt.swapfile = false -- No annoying swapfiles
vim.opt.backup = false -- Disable Vim backups, we have Git :)
vim.opt.undofile = true -- Save undo history
vim.opt.hlsearch = false -- Highlight all matches of a search pattern.
vim.opt.incsearch = true -- Match pattern while typing.
vim.opt.signcolumn = "yes" -- Keep signcolumn always visible
vim.opt.splitbelow = true -- How new splits are created
vim.opt.splitright = true
vim.opt.sw = 4 -- TABs and indentation
vim.opt.ts = 4
vim.opt.expandtab = true
vim.g.netrw_browse_split = 0 -- minimal netrw (vim default file manager)
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.opt.guicursor = ""
vim.opt.timeoutlen = 300 -- vim update time
vim.opt.updatetime = 250
vim.opt.cursorline = false
vim.opt.number = true -- Line numbers
vim.opt.mouse = "a" -- Enable mouse in all modes.
vim.opt.clipboard = "unnamedplus" -- Clipboard
vim.opt.ignorecase = true -- Search has case insensitive by default, but if pattern has some upper case letters, it will be case sensitive.
vim.opt.smartcase = true -- Search has case insensitive by default, but if pattern has some upper case letters, it will be case sensitive.
vim.opt.completeopt = { "fuzzy", "menu", "noinsert", "noselect", "popup" }
vim.opt.inccommand = "" -- Preview all substitutions(replacements).
vim.opt.scrolloff = 10 -- Minimal number of screen lines to keep above and below the cursor.
vim.opt.laststatus = 3 -- Global statusline
vim.keymap.set("n", "Y", "^v$y", { desc = "Copy whole line" })
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
vim.keymap.set("i", "<C-c>", "<esc>")
-- [[ Is there a better way to exit out of insert ? I don't think so.
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
-- ]]
-- [[ When jumping around I want cursor to always remain at the center of the screen.
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-o>", "<C-o>zz")
vim.keymap.set("n", "<C-i>", "<C-i>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
-- ]]
vim.keymap.set("n", "Q", "<cmd>q<CR>")
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("n", "<M-Left>", "<c-w>5>")
vim.keymap.set("n", "<M-Right>", "<c-w>5<")
vim.keymap.set("n", "<M-Up>", "<C-W>+")
vim.keymap.set("n", "<M-Down>", "<C-W>-")
-- [[ Quick fix list
local qflist = false
vim.keymap.set("n", "<C-q>", function()
	if qflist == true then
		qflist = not qflist
		vim.cmd([[ cclose ]])
	else
		qflist = not qflist
		vim.cmd([[ copen ]])
	end
end, { desc = "Open Quickfix list" })
vim.keymap.set("n", "{", "<cmd>cprev<CR>")
vim.keymap.set("n", "}", "<cmd>cnext<CR>")
-- ]]

vim.api.nvim_create_autocmd("TextYankPost", { -- Highlight yanked text
	group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
	callback = function()
		vim.highlight.on_yank()
	end,
})

require("supermaven-nvim").setup({})

local fzfLua = require("fzf-lua")
fzfLua.setup({
	fzf_opts = { ["--layout"] = "default" },
	winopts = { height = 0.5, width = 0.8, row = 0.5, col = 0.5 },
	keymap = {
		fzf = {
			["ctrl-q"] = "select-all+accept",
		},
	},
	defaults = { previewer = false },
})
vim.keymap.set("n", "<leader><leader>", fzfLua.files)
vim.keymap.set("n", "<leader>b", fzfLua.buffers)
vim.keymap.set("n", "<leader>h", fzfLua.help_tags)
vim.keymap.set("n", "<C-p>", fzfLua.git_files)
vim.keymap.set("n", "<leader>p", fzfLua.commands)
vim.keymap.set("n", "??", fzfLua.live_grep)
vim.keymap.set("n", "<leader>.", fzfLua.grep_cWORD)
vim.keymap.set("n", "<leader>,", fzfLua.blines)
vim.keymap.set("v", "<leader>.", fzfLua.grep_visual)
vim.keymap.set("n", "<leader>o", fzfLua.lsp_document_symbols)
vim.keymap.set("n", "<leader>O", fzfLua.lsp_live_workspace_symbols)
vim.keymap.set("n", "<leader>d", fzfLua.git_bcommits)
vim.keymap.set("n", "<leader>s", fzfLua.git_commits)
vim.keymap.set("n", "<leader>a", ":Git blame<CR>")
vim.keymap.set("n", "<leader>i", function()
	fzfLua.files({ cwd = "~/.dotfiles" })
end)

-- treesitter
require("nvim-treesitter.configs").setup({
	auto_install = false,
	sync_install = false,
	ensure_installed = { "lua", "go", "gomod", "markdown", "php", "c", "cpp" },
	ignore_install = {},
	highlight = { enable = true },
	modules = {},
})

require("ts-comments").setup()

-- mason
require("mason").setup({ ensure_installed = { "gopls" } })
local process_path = os.getenv("PATH")
vim.fn.setenv("PATH", process_path .. ":" .. vim.fn.stdpath("data") .. "/mason/bin")

-- LSP setup
local lspconfig = require("lspconfig")
lspconfig.gopls.setup({})
lspconfig.ols.setup({}) -- odin
lspconfig.intelephense.setup({})
lspconfig.rust_analyzer.setup({})
lspconfig.zls.setup({})
lspconfig.lua_ls.setup({
	settings = {
		Lua = {
			telemetry = { enable = false },
			diagnostics = {
				globals = { "vim" },
			},
		},
	},
})
require("conform").setup({
	format_on_save = function()
		if vim.tbl_contains({ "php" }, vim.bo.filetype) then
			return false
		end
		return {
			timeout_ms = 500,
			lsp_format = "fallback",
		}
	end,

	formatters_by_ft = {
		lua = { "stylua", lsp_format = "fallback" },
		go = { "goimports", "gofmt" },
	},
})

vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		local bufnr = args.buf
		vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc", { buf = bufnr })
		local map = function(mode, key, fn, desc)
			vim.keymap.set(mode, key, fn, { buffer = bufnr, desc = "LSP: " .. desc })
		end
		local references = vim.lsp.buf.references
		local implementations = vim.lsp.buf.implementation
		local has_fzf, fzf = pcall(require, "snacks")
		if has_fzf then
			references = fzf.lsp_references
			implementations = fzf.lsp_implementations
		end

		local border = "rounded"
		map("n", "[[", function()
			vim.diagnostic.jump({ count = -1 })
		end, "Diagnostics: Next")
		map("n", "]]", function()
			vim.diagnostic.jump({ count = 1 })
		end, "Diagnostics: Previous")
		map("n", "C-]", vim.lsp.buf.definition, "[g]oto definition")
		map("n", "gd", vim.lsp.buf.definition, "[g]oto [d]efinition")
		map("n", "gD", vim.lsp.buf.declaration, "[g]oto [D]eclaration")
		map("n", "gi", implementations, "[g]oto [i]mplementation")
		map("n", "gr", references, "[g]oto [r]eferences")
		map("n", "R", vim.lsp.buf.rename, "Rename")
		map("n", "K", function()
			vim.lsp.buf.hover({ border = border })
		end, "Hover")
		map("n", "C", vim.lsp.buf.code_action, "Code Actions")
		map({ "n", "i" }, "<C-s>", function()
			vim.lsp.buf.signature_help({ border = border })
		end, "Signature Help")
		map("n", "<leader>l", vim.diagnostic.open_float, "Diagnostics: Open float window")
		map("n", "<leader>q", vim.diagnostic.setloclist, "Set Local list")
		vim.diagnostic.config({
			enabled = true,
			virtual_text = false,
			float = { border = border },
		})
		vim.keymap.set("i", "<c-space>", function()
			vim.lsp.completion.get()
		end)
		vim.keymap.set("i", "<CR>", function()
			return vim.fn.pumvisible() == 1 and "<C-y>" or "<CR>"
		end, { expr = true, noremap = true })

		vim.lsp.completion.enable(true, args.data.client_id, args.buf, { wutotrigger = false }) -- setup completion menu
	end,
})

-- [[ Terminal
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set("t", "<C-w><C-w>", "<cmd>wincmd w<cr>")
local terminal_state = { buf = -1, win = -1, last_tab = -1 }
local terminal_location = "float"
vim.keymap.set({ "n", "t" }, "<c-j>", function()
	if terminal_location == "float" or terminal_location == "bottom" then
		if vim.api.nvim_buf_is_valid(terminal_state.buf) and vim.api.nvim_win_is_valid(terminal_state.win) then
			vim.api.nvim_win_hide(terminal_state.win)
			return
		end
	end

	if terminal_location == "tab" then
		local current_win = vim.api.nvim_get_current_win()
		if vim.wo[current_win].winbar == "Terminal" then
			vim.api.nvim_set_current_tabpage(terminal_state.last_tab)
			return
		end
		for _, tab_id in ipairs(vim.api.nvim_list_tabpages()) do
			local win_id = vim.api.nvim_tabpage_get_win(tab_id)
			local buf_id = vim.api.nvim_win_get_buf(win_id)
			if vim.wo[win_id].winbar == "Terminal" and vim.bo[buf_id].buftype == "terminal" then
				terminal_state.last_tab = vim.api.nvim_get_current_tabpage()
				vim.api.nvim_set_current_tabpage(tab_id)
				vim.cmd.startinsert()
				return
			end
		end
		terminal_state.last_tab = vim.api.nvim_get_current_tabpage()
		vim.cmd.tabnew()
		local win_id = vim.api.nvim_get_current_win()
		vim.wo[win_id].winbar = "Terminal"
		vim.cmd.term()
		vim.cmd.startinsert()
		return
	end

	if not vim.api.nvim_buf_is_valid(terminal_state.buf) then
		print("creating floating term buffer")
		terminal_state.buf = vim.api.nvim_create_buf(false, true)
	end

	if terminal_location == "float" then
		local height = math.floor(vim.o.lines * 0.8)
		local width = math.floor(vim.o.columns * 0.8)

		local row = math.floor((vim.o.lines - height) / 2)
		local col = math.floor((vim.o.columns - width) / 2)

		terminal_state.win = vim.api.nvim_open_win(terminal_state.buf, true, {
			relative = "editor",
			width = width,
			height = height,
			row = row,
			col = col,
			style = "minimal",
			border = "rounded",
		})
	elseif terminal_location == "bottom" then
		local width = vim.o.columns
		local height = math.floor(vim.o.lines * 0.45)
		terminal_state.win = vim.api.nvim_open_win(terminal_state.buf, true, {
			split = "below",
			width = width,
			height = height,
		})
	else
		vim.error("Invalid location for terminal")
		return
	end

	if vim.api.nvim_get_option_value("buftype", { buf = terminal_state.buf }) ~= "terminal" then
		vim.cmd.term()
	end

	vim.cmd.startinsert()
end)
-- ]]
