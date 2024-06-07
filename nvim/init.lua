--    ___         _                      ___       __
--   / _ | __ _  (_)__________ ___ ___ _/ _ | ___ / /__
--  / __ |/  ' \/ / __/ __/ -_)_ // _ `/ __ |(_-</  '_/
-- /_/ |_/_/_/_/_/_/ /_/  \__//__/\_,_/_/ |_/___/_/\_\
-- Minimal, fast configuration for neovim.

-- Wrap long lines
vim.opt.wrap = true

-- Wrapped lines have same indentation as the actual line.
vim.opt.breakindent = true

-- No annoying swapfiles
vim.opt.swapfile = false

-- Disable Vim backups, we have Git :)
vim.opt.backup = false

-- Save undo history
vim.opt.undofile = true

-- Highlight all matches of a search pattern.
vim.opt.hlsearch = false

-- Match pattern while typing.
vim.opt.incsearch = true

-- Keep signcolumn always visible
vim.opt.signcolumn = "yes"

-- How new splits are created
vim.opt.splitbelow = true
vim.opt.splitright = true

-- TABs and indentation
vim.opt.sw = 4
vim.opt.ts = 4
vim.opt.expandtab = true

-- minimal netrw (vim default file manager)
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25

-- vim update time
vim.opt.timeoutlen = 300
vim.opt.updatetime = 250

-- Line numbers
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.mouse = "a"

-- No need to show the mode, we have it in statusline
vim.opt.showmode = false

-- Case-insensitive searching UNLESS \C or capital in search
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Preview all substitutions(replacements).
vim.opt.inccommand = "split"

-- Minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 10

-- Diagnostics
vim.diagnostic.config({
	float = {
		border = "rounded",
	},
})

vim.opt.cursorline = true

-- Statusline
function AmirrezaStatusLine()
	local mode_texts = {
		n = "Normal",
		i = "Insert",
		v = "Visual",
		c = "Command",
	}
	local statusline = ""
	local mode = vim.api.nvim_get_mode().mode
	if mode_texts[mode] ~= nil then
		mode = mode_texts[mode]
	end

	statusline = statusline .. string.format("[%s]", mode) .. " "

	local branch = ""
	if vim.b.gitsigns_head ~= nil then
		branch = "[Git:" .. vim.b.gitsigns_head .. "]"
		statusline = statusline .. branch
	end

	return statusline .. " %r%h%w%q%m%f %l:%c %y"
end

vim.opt.statusline = "%!v:lua.AmirrezaStatusLine()"

IS_WINDOWS = vim.fn.has("win32") == 1

-- Highlight on Yank
vim.api.nvim_create_autocmd("TextYankPost", {
	group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
	callback = function()
		vim.highlight.on_yank()
	end,
})

vim.api.nvim_create_user_command("Term", function()
	vim.cmd([[ tabnew | term ]])
end, {})

vim.api.nvim_create_user_command("VTerm", function()
	vim.cmd([[ vnew | term ]])
end, {})

-- <leader> key for keymaps mapped to <Space>
vim.g.mapleader = " "

-- Clipboard
vim.opt.clipboard = "unnamedplus"

-- Make yanking act like other operations
vim.keymap.set("n", "Y", "y$", { desc = "Copy whole line" })

-- Esc should remove incsearch highlights
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- Simpler exiting insert mode
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")

-- Quickfix list
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

vim.keymap.set({ "i", "n" }, "<C-k>", "<cmd>cprev<CR>")
vim.keymap.set({ "i", "n" }, "<C-j>", "<cmd>cnext<CR>")

-- Tab
vim.keymap.set({ "n" }, "<leader>j", "<cmd>tabprev<CR>")
vim.keymap.set({ "n" }, "<leader>k", "<cmd>tabnext<CR>")

vim.keymap.set({ "n" }, "<C-q>", ToggleQFList, { desc = "Open Quickfix list" })
vim.keymap.set({ "i" }, "<C-Space>", "<C-x><C-o>")

-- When moving around always have cursor centered in screen
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-o>", "<C-o>zz")
vim.keymap.set("n", "<C-i>", "<C-i>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")

-- Disable annoying Q mode
vim.keymap.set("n", "Q", "<cmd>q<CR>")

-- Pressing <CR> will disable current highlights from last search
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })

-- Edit config file
vim.keymap.set("n", "<leader>i", "<cmd>edit $MYVIMRC<CR>")

-- Wrapped lines act as normal lines
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")

-- Terminal and Tabs
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set("t", "<C-w><C-w>", function()
	vim.cmd([[ wincmd w ]])
end)
vim.keymap.set({ "i" }, "<C-a>", "<C-x><C-o>") -- simpler omnifunc completion

-- Diagnostics
vim.keymap.set("n", "<leader>l", vim.diagnostic.open_float, { desc = "Diagnostics: Open float window" })
vim.keymap.set("n", "[[", vim.diagnostic.goto_prev, { desc = "Diagnostics: Next" })
vim.keymap.set("n", "]]", vim.diagnostic.goto_next, { desc = "Diagnostics: Previous" })
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Set Local list" })

-- W is alias for w
vim.cmd([[
    command! W :w
]])

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

local TRANSPARENT = false

vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	-- Detect tabstop and shiftwidth automatically.
	{ "tpope/vim-sleuth" },

	-- [gc] to comment region/line.
	{
		"folke/ts-comments.nvim",
		event = "VeryLazy",
		opts = {},
	},
	{
		"folke/noice.nvim",
		event = "VeryLazy",
		opts = {},
		dependencies = {
			"MunifTanjim/nui.nvim",
			"rcarriga/nvim-notify",
		},
	},
	-- Highlight TODO in comments
	{ "folke/todo-comments.nvim", opts = {} },

	-- Search and replace in files
	{
		"nvim-pack/nvim-spectre",
		config = function()
			vim.keymap.set("n", "<leader>sr", function()
				require("spectre").open()
			end)
		end,
	},

	-- Colorschemes
	{
		"rose-pine/neovim",
		name = "rose-pine",
		opts = {
			styles = {
				italic = false,
				transparency = TRANSPARENT,
			},
		},
	},
	{
		"folke/tokyonight.nvim",
		opts = {
			transparent = TRANSPARENT,
		},
	},
	{
		"catppuccin/nvim",
		name = "catppuccin",
		opts = {
			transparent_background = TRANSPARENT,
		},
	},
	{
		"ellisonleao/gruvbox.nvim",
		config = function()
			require("gruvbox").setup({
				contrast = "hard",
				overrides = {
					Comment = { fg = "#2ea542" },
				},
				italic = {
					strings = false,
					emphasis = false,
					comments = false,
					operators = false,
					folds = false,
				},
			})
		end,
	},

	-- Icons
	{ "nvim-tree/nvim-web-devicons", lazy = true },

	-- ui components
	{ "MunifTanjim/nui.nvim", lazy = true },

	{
		"nvim-lualine/lualine.nvim",
		opts = {},
	},

	{
		"rcarriga/nvim-notify",
		opts = {},
	},

	-- indent guides for Neovim
	{
		"lukas-reineke/indent-blankline.nvim",
		opts = {
			scope = { enabled = false },
			indent = {
				char = "│",
				tab_char = "│",
			},
			exclude = {
				filetypes = {
					"help",
					"alpha",
					"dashboard",
					"neo-tree",
					"Trouble",
					"trouble",
					"lazy",
					"mason",
					"notify",
					"toggleterm",
					"lazyterm",
				},
			},
		},
		main = "ibl",
	},

	-- Show git changed signs next to line numbers.
	{
		"lewis6991/gitsigns.nvim",
		opts = {
			signs = {
				add = { text = "+" },
				change = { text = "~" },
				delete = { text = "_" },
				topdelete = { text = "‾" },
				changedelete = { text = "~" },
			},
		},
	},

	-- Terminal Emulator inside neovim inside terminal :))
	{
		"akinsho/toggleterm.nvim",
		version = "*",
		opts = {
			open_mapping = "<C-l>",
		},
	},

	-- File manager
	{
		"stevearc/oil.nvim",
		opts = {},
	},

	-- Git Client
	{ "tpope/vim-fugitive" },

	-- Autocomplete popup
	{
		"hrsh7th/nvim-cmp",
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-vsnip",
			"hrsh7th/vim-vsnip",
		},
		config = function()
			local cmp_select = { behavior = require("cmp").SelectBehavior.Select }
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
					["<CR>"] = cmp.mapping.confirm({ select = true }),
					["<C-Space>"] = cmp.mapping.complete(),
				}),
				sources = {
					{ name = "nvim_lsp" },
					{ name = "buffer" },
					{ name = "path" },
				},
			})
		end,
	},

	{ -- Autoformat
		"stevearc/conform.nvim",
		opts = {
			notify_on_error = false,
			format_on_save = {
				timeout_ms = 500,
				lsp_fallback = true,
			},
			formatters_by_ft = {
				lua = { "stylua" },
				go = { "goimports" },
			},
		},
	},

	{ -- Fuzzy finder
		"nvim-telescope/telescope.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
			},
			"nvim-telescope/telescope-ui-select.nvim",
		},
		config = function()
			require("telescope").load_extension("ui-select") -- Use telescope for vim.ui.select
			local builtin = require("telescope.builtin")
			local no_preview = { previewer = false }
			local dropdown = require("telescope.themes").get_dropdown
			local map = function(mode, key, fn, desc)
				vim.keymap.set(mode, key, fn, { desc = "Telescope: " .. desc })
			end

			local projects_root = "~/w"
			if IS_WINDOWS then
				projects_root = "C:/w"
			end

			local function find_projects()
				local repos = vim.fs.find({ ".git" }, { limit = math.huge, path = projects_root })
				local paths = {}
				for _, repo in ipairs(repos) do
					table.insert(paths, vim.fs.dirname(repo))
				end

				return paths
			end

			map("n", "<C-p>", function()
				builtin.git_files(dropdown({
					previewer = false,
					prompt_title = string.format("Git Files: %s", vim.fn.getcwd()),
				}))
			end, "Git Files")

			map("n", "<leader><CR>", function()
				vim.ui.select(find_projects(), {
					prompt = "Select Project:",
				}, function(proj)
					if proj == "" or proj == nil then
						return
					end
					builtin.find_files({ previewer = false, cwd = proj })
				end)
			end, "Find File in project")

			map("n", "<leader><leader>", function()
				builtin.find_files({
					previewer = false,
					prompt_title = string.format("Find Files: %s", vim.fn.getcwd()),
				})
			end, "Fuzzy Find in current buffer project")

			map("n", "<leader>b", function()
				builtin.buffers({ previewer = false })
			end, "Buffers")

			map("n", "<leader>/", function()
				builtin.current_buffer_fuzzy_find({ previewer = false })
			end, "Fuzzy find in current buffer")

			map("n", "<leader>.", function()
				builtin.grep_string({ previewer = false, layout_config = { height = 0.7, width = 0.9 } })
			end, "Grep current word")

			map("n", "<leader>fw", function()
				vim.ui.input({ prompt = "Grep> " }, function(s)
					if s ~= "" then
						builtin.grep_string({
							search = s,
							previewer = false,
							layout_config = { height = 0.7, width = 0.9 },
						})
					end
				end)
			end, "Grep Word")

			map("n", "<leader>o", function()
				builtin.treesitter(no_preview)
			end, "Treesitter symbols")

			map("n", "??", function()
				builtin.live_grep({
					previewer = false,
					prompt_title = string.format("Grep: %s", vim.fn.getcwd()),
					layout_config = { height = 0.9, width = 0.9 },
				})
			end, "Grep in project")

			map("n", "<leader>h", function()
				builtin.help_tags()
			end, "Help Tags")

			map("n", "<leader>w", function()
				builtin.lsp_dynamic_workspace_symbols()
			end, "LSP workspace symbols")
		end,
	},

	{ -- Language server protocol client (LSP)
		"neovim/nvim-lspconfig",
		dependencies = {
			{ -- Like the panel in vscode, shows you errors and warnings from LSP
				"folke/trouble.nvim",
				-- dependencies = { "nvim-tree/nvim-web-devicons" },
				config = function()
					require("trouble").setup({})
					vim.keymap.set("n", "<leader>e", ":TroubleToggle<CR>")
				end,
			},
			{ "folke/neodev.nvim", opts = {} },

			{ -- Package manager for neovim install lsp servers in neovim path.
				"williamboman/mason.nvim",
				config = function()
					local function get_path_sep()
						if IS_WINDOWS then
							return "\\"
						else
							return "/"
						end
					end

					local sep = get_path_sep()

					if IS_WINDOWS then
						vim.env.PATH = string.format("%s%smason%sbin;", (vim.fn.stdpath("data")), sep, sep)
							.. vim.env.PATH
					else
						vim.env.PATH = string.format("%s%smason%sbin:", (vim.fn.stdpath("data")), sep, sep)
							.. vim.env.PATH
					end
					require("mason").setup({})
				end,
			},
			{ "j-hui/fidget.nvim", opts = {} },
		},
		config = function()
			local lsp_servers = {
				gopls = {},
				intelephense = {},
				lua_ls = {
					settings = {
						Lua = {
							telemetry = { enable = false },
							diagnostics = {
								globals = { "vim" },
							},
							workspace = {
								checkThirdParty = false,
								library = {
									"${3rd}/luv/library",
									unpack(vim.api.nvim_get_runtime_file("", true)),
								},
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

			-- LspInfo window have rounded border
			require("lspconfig.ui.windows").default_options.border = "single"
			vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" })
			vim.lsp.handlers["textDocument/signatureHelp"] =
				vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" })
			vim.api.nvim_create_autocmd("LspAttach", {
				callback = function(args)
					local bufnr = args.buf
					vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc", { buf = bufnr })

					local map = function(mode, key, fn, desc)
						vim.keymap.set(mode, key, fn, { buffer = bufnr, desc = "LSP: " .. desc })
					end
					local references = vim.lsp.buf.references
					local implementation = vim.lsp.buf.implementation
					local has_tele, tele = pcall(require, "telescope.builtin")
					if has_tele then
						references = tele.lsp_references
						implementation = tele.lsp_implementations
					end
					map("n", "gd", vim.lsp.buf.definition, "[g]oto [d]efinition")
					map("n", "gD", vim.lsp.buf.declaration, "[g]oto [D]eclaration")
					map("n", "gI", implementation, "[g]oto [i]mplementation")
					map("n", "gr", references, "[g]oto [r]eferences")
					map("n", "R", vim.lsp.buf.rename, "Rename")
					map("n", "K", vim.lsp.buf.hover, "Hover")
					map("n", "C", vim.lsp.buf.code_action, "Code Actions")
					map("n", "<leader>f", vim.lsp.buf.format, "Format")
					map({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, "Signature Help")

					-- I hate it when I am writing a piece of code that things start to get all red.
					vim.diagnostic.config({ virtual_text = false })
				end,
			})
		end,
	},

	{ -- Treesitter, see :help treesitter
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			require("nvim-treesitter.configs").setup({
				-- A list of parser names or tiers ('stable', 'core', 'community', 'unsupported')
				ensure_installed = { "lua", "go", "gomod", "markdown", "php", "c", "cpp" },

				-- List of parsers to ignore installing
				ignore_install = { "unsupported" },

				-- Automatically install missing parsers when entering buffer
				auto_install = false,

				-- Directory to install parsers and queries to
				install_dir = vim.fn.stdpath("data") .. "/site",
			})
			vim.api.nvim_create_autocmd("FileType", {
				callback = function()
					pcall(vim.treesitter.start)
				end,
			})
		end,
	},
}, {})

-- Change current working directory based on the buffer project root.
-- local augroup = vim.api.nvim_create_augroup("amirreza-chcwd", {})
-- vim.api.nvim_create_autocmd("BufEnter", {
-- 	callback = function(ev)
-- 		local filename = ev.file
-- 		local start_from = vim.fs.dirname(filename)
--
-- 		local root = vim.fs.dirname(
-- 			vim.fs.find({ ".git", "go.mod", "package.json", "cargo.toml" }, { upward = true, path = start_from })[1]
-- 		)
-- 		if root ~= nil and root ~= "" then
-- 			local abs_path = require("plenary.path").new(root or vim.fn.getcwd()):absolute()
-- 			vim.fn.chdir(abs_path)
-- 		end
-- 	end,
-- 	group = augroup,
-- })

vim.cmd.colorscheme("tokyonight-night")
