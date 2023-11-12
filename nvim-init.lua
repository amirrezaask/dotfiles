--     ___              _                            ___         __
--    /   |  ____ ___  (_)____________  ____  ____ _/   |  _____/ /__
--   / /| | / __ `__ \/ / ___/ ___/ _ \/_  / / __ `/ /| | / ___/ //_/
--  / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ / ___ |(__  ) ,<
-- /_/  |_/_/ /_/ /_/_/_/  /_/   \___/ /___/\__,_/_/  |_/____/_/|_|
-- AmirrezaAsk neovim configuration



-- ====================================================
-- Options
-- ====================================================
vim.opt.number = true         -- Line numbers
vim.opt.relativenumber = true -- Relative line numbers
vim.opt.errorbells = false
vim.opt.smartindent = true
vim.opt.wrap = true
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = true
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.termguicolors = true
vim.opt.signcolumn = "yes"
vim.opt.isfname:append("@-@")
vim.opt.updatetime = 50
vim.opt.shortmess:append("c") -- Don't pass messages to |ins-completion-menu|.
vim.opt.shortmess:append("I") -- No Intro message
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.cursorline = true
vim.opt.sw = 4
vim.opt.ts = 4
vim.opt.expandtab = true
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.opt.laststatus = 2
vim.opt.timeoutlen = 300
vim.opt.laststatus = 3

-- ====================================================
-- Keybindings
-- ====================================================
vim.g.mapleader = " "
vim.keymap.set("n", "Y", "y$", { desc = "Copy whole line" })                         -- Make yanking act like other operations
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]], { desc = "Copy into clipboard" }) -- Copy to clipboard
vim.keymap.set("n", "<leader>Y", [["+Y]], { desc = "Copy line into clipboard" })
vim.keymap.set("n", "<leader>p", [["+p]], { desc = "Paste from clipboard" })
-- If I visually select words and paste from clipboard, don't replace my
-- clipboard with the selected word, instead keep my old word in the clipboard
vim.keymap.set("x", "p", '"_dP')
-- Simpler exiting insert mode
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
-- splits
vim.keymap.set("n", "<leader>k", ":vsplit<cr>")
vim.keymap.set("n", "<leader>j", ":split<cr>")
-- Quickfix list
vim.keymap.set({ "n" }, "<C-[>", "<cmd>cprev<CR>", { desc = "Previous quick fix list item" })
vim.keymap.set({ "n" }, "<C-]>", "<cmd>cnext<CR>", { desc = "Next quick fix list item" })
local qflist = false
function ToggleQFList()
	if qflist == true then
		qflist = not qflist
		vim.cmd([[ cclose ]])
	else
		qflist = not qflist
		vim.cmd([[ copen ]])
	end
end

vim.keymap.set({ "n" }, "<C-q>", ToggleQFList, { desc = "Open Quickfix list" })
-- When moving around always have cursor centered in screen
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("n", "Q", "<cmd>q<CR>")
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true }) -- handy when doing search in a buffer

-- Wrapped lines act as normal lines
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")

-- Terminal and Tabs
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set({ "i", "n", "t" }, "<C-k>", "<cmd>tabnext<CR>")
vim.keymap.set({ "i", "n", "t" }, "<C-j>", "<cmd>tabprev<CR>")

-- ====================================================
-- Lazy package manager
-- ====================================================
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)


-- ====================================================
-- Plugins
-- ====================================================

require "lazy".setup {
	{
		"lewis6991/gitsigns.nvim",
		config = function()
			require("gitsigns").setup({
				signs = {
					add = { text = "+" },
					change = { text = "~" },
					delete = { text = "_" },
					topdelete = { text = "‾" },
					changedelete = { text = "~" },
				},
			})

			vim.keymap.set("n", "<leader>l", "<cmd>Gitsigns blame_line<CR>")
		end,
	},
	"tpope/vim-fugitive",
	"junegunn/gv.vim",

	"tpope/vim-abolish",             -- useful text stuff
	{ "numToStr/Comment.nvim",    opts = {} }, -- Comment stuff like a boss
	"fladson/vim-kitty",             -- Support Kitty terminal config syntax
	"towolf/vim-helm",               -- Support for helm template syntax
	"jansedivy/jai.vim",             -- Jai from Jonathan Blow
	"tpope/vim-sleuth",

	-- nvim-cmp: autocompletion
	{ "hrsh7th/nvim-cmp",
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-vsnip",
			"hrsh7th/vim-vsnip",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-buffer",
		},
		config = function()
			local cmp_select = { behavior = require("cmp").SelectBehavior.Select }
			local capabilities = vim.lsp.protocol.make_client_capabilities()
			capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)
			local cmp = require("cmp")
			cmp.setup({
				preselect = require("cmp.types").cmp.PreselectMode.None,
				window = {
					completion = cmp.config.window.bordered(),
					documentation = cmp.config.window.bordered(),
				},
				snippet = {
					expand = function(args)
						vim.fn["vsnip#anonymous"](args.body)
					end,
				},
				mapping = cmp.mapping.preset.insert({
					["<C-p>"] = cmp.mapping.select_prev_item(cmp_select),
					["<C-n>"] = cmp.mapping.select_next_item(cmp_select),
					["<C-y>"] = cmp.mapping.confirm({ select = true }),
					["<C-Space>"] = cmp.mapping.complete(),
					["<CR>"] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
				}),
				sources = {
					{ name = "nvim_lsp" },
					{ name = "buffer" },
					{ name = "path" },
				},
			})
		end,
	},

	{
		'nvim-lualine/lualine.nvim',
		requires = { 'nvim-tree/nvim-web-devicons', opt = true },
		opts = {}
	},
	-- Treesitter
	{
		"nvim-treesitter/nvim-treesitter",
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects",
			"nvim-treesitter/playground",
		},
		config = function()
			require("nvim-treesitter.configs").setup({
				sync_install = false,
				auto_install = true,
				ignore_install = {},
				modules = {},
				ensure_installed = {
					"json",
					"yaml",
					"c",
					"cpp",
					"lua",
					"rust",
					"go",
					"python",
					"php",
					"ocaml",
					"sql",
				},
				context_commentstring = { enable = true },
				highlight = { enable = true, additional_vim_regex_highlighting = false },
				textobjects = {
					select = {
						enable = true,
						lookahead = true,
						keymaps = {
							["af"] = "@function.outer",
							["if"] = "@function.inner",
							["ac"] = "@class.outer",
							["ic"] = "@class.inner",
						},
					},
				},
			})

			-- Install all treesitter parsers.
			pcall(require("nvim-treesitter.install").update({ with_sync = true }))
		end,
	},
	-- Colorschemes
	{ "rose-pine/neovim",         name = "rose-pine",          opts = { disable_italics = true } },
	{ "ellisonleao/gruvbox.nvim", opts = { contrast = 'hard' } },
	{ 'navarasu/onedark.nvim',    opts = { style = 'darker' } },
	{ "catppuccin/nvim",          name = "catppuccin",         opts = { flavor = "macchiato" } },
	{ "EdenEast/nightfox.nvim" },
	{

		"nvim-telescope/telescope.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
			"nvim-telescope/telescope-ui-select.nvim",
		},
		config = function()
			require("telescope").setup({
				defaults = {
					sorting_strategy = "ascending",
					layout_strategy = "horizontal",
					layout_config = {
						horizontal = {
							preview_cutoff = 180,
							prompt_position = "top",
							height = 0.6,
							width = 0.7,
							preview_width = 0.7,
						},
					},
				},
			})

			require("telescope").load_extension("fzf") -- load fzf awesomnes into Telescope
			require("telescope").load_extension("ui-select") -- Use telescope for vim.ui.select
			local telescope_builtin = require("telescope.builtin")
			local no_preview = { previewer = false }

			vim.keymap.set("n", "<C-p>", function()
				telescope_builtin.git_files(no_preview)
			end, { desc = "Git Files" })
			vim.keymap.set("n", "<leader>b", function()
				telescope_builtin.buffers(no_preview)
			end, { desc = "Telescope Buffers" })
			vim.keymap.set("n", "<leader><leader>", function()
				telescope_builtin.find_files(no_preview)
			end, { desc = "Find Files" })
			vim.keymap.set("n", "<leader>ff", function()
				telescope_builtin.find_files(no_preview)
			end, { desc = "Find Files" })
			vim.keymap.set("n", "<leader>w", function()
				telescope_builtin.grep_string({ layout_config = { height = 0.7, width = 0.9 } })
			end, { desc = "Grep for word at point" })
			vim.keymap.set("n", "<leader>o", function()
				telescope_builtin.treesitter(no_preview)
			end, { desc = "Search Symbols In Current File" })
			vim.keymap.set("n", "??", function()
				telescope_builtin.live_grep({ layout_config = { height = 0.9, width = 0.9 } })
			end, { desc = "Live Grep" })
		end,
	},
	{

		"neovim/nvim-lspconfig",
		dependencies = { "williamboman/mason.nvim" },
		config = function()
			local function get_path_sep()
				if vim.fn.has("win32") == 1 then
					return "\\"
				else
					return "/"
				end
			end

			local sep = get_path_sep()

			vim.env.PATH = string.format("%s%smason%sbin:", (vim.fn.stdpath("data")), sep, sep) ..
			vim.env.PATH
			require("mason").setup({})
			local lsp_servers = {
				ocamllsp = {
					cmd = { string.format("%s/.opam/default/bin/%s", os.getenv("HOME"), "ocamllsp") },
					get_language_id = function(_, ftype)
						return ftype
					end,
				},
				gopls = {},
				lua_ls = {
					settings = {
						Lua = {
							telemetry = { enable = false },
							diagnostics = {
								globals = { "vim" },
							},
							workspace = {
								checkThirdParty = false,
								library = vim.api.nvim_get_runtime_file("", true),
							},
						},
					},
				},
				rust_analyzer = {},
				zls = {},
			}
			for server, config in pairs(lsp_servers) do
				require("lspconfig")[server].setup(config)
			end
			vim.diagnostic.config({ virtual_text = true })

			vim.api.nvim_create_autocmd("LspAttach", {
				callback = function(args)
					local bufnr = args.buf
					vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc",
						{ buf = bufnr })
					local buffer = function(desc)
						return { buffer = bufnr, desc = desc }
					end
					vim.keymap.set("n", "gd", vim.lsp.buf.definition, buffer("Goto Definition"))
					vim.keymap.set("n", "gD", vim.lsp.buf.declaration, buffer("Goto Declaration"))
					vim.keymap.set("n", "gi", vim.lsp.buf.implementation,
						buffer("Goto Implementation"))
					vim.keymap.set("n", "gr", vim.lsp.buf.references, buffer("Goto References"))
					vim.keymap.set("n", "R", vim.lsp.buf.rename, buffer("Rename"))
					vim.keymap.set("n", "K", vim.lsp.buf.hover, buffer("Hover"))
					vim.keymap.set("n", "<leader>f", vim.lsp.buf.format, buffer("Format"))
					vim.keymap.set("n", "gl", vim.diagnostic.open_float, buffer(""))
					vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, buffer("Next Diagnostic"))
					vim.keymap.set("n", "]d", vim.diagnostic.goto_next, buffer("Previous Diagnostic"))
					vim.keymap.set("n", "C", vim.lsp.buf.code_action, buffer("Code Actions"))
					vim.keymap.set("n", "<C-s>", vim.lsp.buf.signature_help, buffer("Signature Help"))
					vim.keymap.set("i", "<C-s>", vim.lsp.buf.signature_help, buffer("Signature Help"))
				end,
			})
			-- Hover and signature help windows have rounded borders
			vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover,
				{ border = "rounded" })
			vim.lsp.handlers["textDocument/signatureHelp"] =
			    vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" })

			-- LspInfo window have rounded border
			require("lspconfig.ui.windows").default_options.border = "rounded"
		end,
	}
}


-- ====================================================
-- Golang format
-- ====================================================
vim.api.nvim_create_autocmd("BufWritePre", {
	pattern = "*.go",
	callback = function()
		vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } }, apply = true })
		vim.lsp.buf.format()
	end,
})

-- ====================================================
-- Colorscheme setup
-- ====================================================

local color_state = 'dark'
local dark_colorscheme = 'rose-pine'
local light_colorscheme = 'rose-pine-dawn'

vim.cmd.colorscheme(dark_colorscheme)
function ToggleColorscheme()
	if color_state == 'dark' then
		color_state = 'light'
		vim.cmd.colorscheme(light_colorscheme)
	else
		color_state = 'light'
		vim.cmd.colorscheme(dark_colorscheme)
	end
end
