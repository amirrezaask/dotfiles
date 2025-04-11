local COLORSCHEME = vim.env.NVIM_COLORSCHEME or "rose-pine-moon"
local TRANSPARENT = vim.env.NVIM_TRANSPARENT == true or true
local INDENT_LINES = vim.env.NVIM_INDENT_LINES == true or false

function printf(...)
	local args = { ... }
	print(string.format(...))
end

function RELOAD(module)
	package.loaded[module] = nil
	return require(module)
end

vim.g.mapleader = " " -- <leader> key for keymaps mapped to <Space>
vim.o.wrap = true -- Wrap long lines
vim.o.breakindent = true -- Wrapped lines have same indentation as the actual line.
vim.o.swapfile = false -- No annoying swapfiles
vim.o.backup = false -- Disable Vim backups, we have Git for that :)
vim.o.undofile = true -- Save undo history
vim.o.hlsearch = false -- Highlight all matches of a search pattern.
vim.o.incsearch = true -- Match pattern while typing.
vim.o.signcolumn = "yes" -- Keep signcolumn always visible
vim.o.cursorline = false -- Highlights current line with hl defined as *hl-CursorLine*
vim.o.splitbelow = true -- How new splits are created
vim.o.splitright = true -- SAME
vim.o.showmode = false -- don't show --INSERT-- in command line.
vim.o.sw = 4 -- TABs and indentation
vim.o.ts = 4 -- TABS and indentation
vim.o.expandtab = true -- TABs and indentation
vim.o.guicursor = "" -- I don't want my cursor shape change with mode changes.
vim.o.timeoutlen = 300 -- Time vim waits for a key sequence to finish.
vim.o.updatetime = 250 -- Milliseconds to wait for CursorHold autocmds to fire.
vim.o.number = true -- Line numbers
vim.o.mouse = "a" -- Enable mouse in all modes.
vim.o.clipboard = "unnamedplus" -- Clipboard
vim.o.ignorecase = true -- Search has case insensitive by default, but if pattern has some upper case letters, it will be case sensitive.
vim.o.smartcase = true -- Search has case insensitive by default, but if pattern has some upper case letters, it will be case sensitive.
vim.o.completeopt = "fuzzy,menu,noinsert,noselect,popup"
vim.o.inccommand = "" -- Preview all substitutions(replacements).
vim.o.scrolloff = 10 -- Minimal number of screen lines to keep above and below the cursor.
vim.o.laststatus = 3
vim.o.statusline = "%l:%c %m%r%q%h%f"
vim.keymap.set("n", "Y", "^v$y", { desc = "Copy whole line" })
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-o>", "<C-o>zz")
vim.keymap.set("n", "<C-i>", "<C-i>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("n", "Q", "<cmd>q<CR>")
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
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
	{
		"stevearc/conform.nvim",
		opts = {
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
		},
	},
	{
		"stevearc/oil.nvim",
		opts = {},
		dependencies = { "nvim-tree/nvim-web-devicons" },
	},
	{
		"neovim/nvim-lspconfig",
		config = function()
			-- LSP setup
			local lspconfig = require("lspconfig")
			for _, lsp in ipairs({ "gopls", "ols", "intelephense", "rust_analyzer", "zls" }) do
				lspconfig[lsp].setup {}
			end
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
			vim.api.nvim_create_autocmd("LspAttach", {
				callback = function(args)
					local bufnr = args.buf
					vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc", { buf = bufnr })
					local map = function(mode, key, fn, desc)
						vim.keymap.set(mode, key, fn, { buffer = bufnr, desc = "LSP: " .. desc })
					end
					local references = require("snacks").picker.lsp_references
					local implementations = require("snacks").picker.lsp_implementations

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

					vim.keymap.set("i", "<CR>", function()
						return vim.fn.pumvisible() == 1 and "<C-y>" or "<CR>"
					end, { expr = true, noremap = true })

					vim.lsp.completion.enable(true, args.data.client_id, args.buf, { wutotrigger = false }) -- setup completion menu
				end,
			})
		end,
	},
	{
		"williamboman/mason.nvim",
		config = function()
			require("mason").setup({ ensure_installed = { "gopls" } })
			local process_path = os.getenv("PATH")
			vim.fn.setenv("PATH", process_path .. ":" .. vim.fn.stdpath("data") .. "/mason/bin")
		end,
	},
	{
		"saghen/blink.cmp",
		version = "1.*",
		opts = {
			keymap = { preset = "enter" },
			cmdline = { enabled = false },
		},
	},
	{
		"nvim-treesitter/nvim-treesitter",
		config = function()
			require("nvim-treesitter.configs").setup {
				auto_install = false,
				sync_install = false,
				ensure_installed = { "lua", "go", "gomod", "markdown", "php", "c", "cpp" },
				ignore_install = {},
				highlight = { enable = true },
				modules = {},
			}
		end,
	},
	{ "folke/ts-comments.nvim", opts = {} },
	{
		"folke/snacks.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			snacks = require("snacks")
			snacks.setup {
				bigfile = { enabled = true },
				picker = {
					enabled = true,
				},
				indent = { enabled = INDENT_LINES },
				explorer = { enabled = true },
				terminal = { enabled = true },
			}
			p = require("snacks").picker
			vim.keymap.set({ "n", "i" }, "<C-e>", function()
				snacks.explorer()
			end, {})
			vim.keymap.set({ "n", "i", "t" }, "<C-j>", snacks.terminal.toggle, {})

			vim.api.nvim_create_autocmd(
				"FileType",
				{ -- Maybe a better way is to have a static key bound to a function that runs some terminal command based on the foreg founding of go.mod.
					pattern = "go",
					callback = function()
						vim.keymap.set("n", "<M-m>", function()
							snacks.terminal.get("go build -v ./...", { cwd = vim.fn.getcwd(), auto_close = false })
						end)
					end,
				}
			)
			vim.keymap.set("n", "<leader><leader>", p.files, {})
			vim.keymap.set("n", "<leader>ff", p.files, {})
			vim.keymap.set("n", "<C-p>", p.git_files, {})
			vim.keymap.set("n", "<leader>fg", p.git_files, {})
			vim.keymap.set("n", "<leader>fd", function()
				p.files { cwd = "~/.dotfiles" }
			end, {})
			vim.keymap.set("n", "??", p.grep, {})
			vim.keymap.set("n", "<leader>fb", p.buffers, {})
			vim.keymap.set("n", "<leader>h", p.help, {})
			vim.keymap.set("n", "<leader>d", p.diagnostics_buffer, {})
			vim.keymap.set("n", "<leader>D", p.diagnostics, {})
			vim.keymap.set("n", "<leader>o", p.lsp_symbols, {})
			vim.keymap.set("n", "<leader>O", p.lsp_workspace_symbols, {})
		end,
	},
	{ "folke/tokyonight.nvim" },
	{ "rose-pine/neovim", name = "rose-pine" },
	{ "catppuccin/nvim", name = "catppuccin" },
}

function Transparent()
	vim.cmd [[
        hi! Normal      guibg=none
        hi! NormalNC    guibg=none
        hi! NormalFloat guibg=none
        hi! FloatBorder guibg=none
        hi! FloatTitle  guibg=none
        hi! FloatFooter guibg=none
        hi! SignColumn  guibg=none
        hi! LineNr      guibg=none
    ]]
end

vim.cmd.colorscheme(COLORSCHEME)
if TRANSPARENT then
	Transparent()
end
