local transparent = os.getenv("NVIM_TRANSPARENT") or true

vim.g.mapleader = " "

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

require("lazy").setup({
	{ -- AI apocalypse is here !!!!
		"supermaven-inc/supermaven-nvim",
		config = function()
			require("supermaven-nvim").setup({})
		end,
	},
	{
		"ibhagwan/fzf-lua",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		enabled = false,
		config = function()
			local fzfLua = require("fzf-lua")
			fzfLua.setup({
				winopts = {
					-- split = "belowright new",
					-- fullscreen = true,
				},
				keymap = {
					fzf = {
						["ctrl-q"] = "select-all+accept",
					},
				},
				defaults = {
					previewer = false,
				},
				commands = {
					actions = {
						["enter"] = function(selected)
							vim.cmd(selected[1])
						end,
					},
				},
			})
			vim.keymap.set("n", "<leader><leader>", fzfLua.files)
			vim.keymap.set("n", "<leader>b", fzfLua.buffers)
			vim.keymap.set("n", "<leader>h", fzfLua.help_tags)
			vim.keymap.set("n", "<C-p>", fzfLua.git_files)
			vim.keymap.set("n", "??", fzfLua.live_grep)
			vim.keymap.set("n", "<leader>o", fzfLua.lsp_document_symbols)
			vim.keymap.set("n", "<leader>O", fzfLua.lsp_live_workspace_symbols)
			vim.keymap.set("n", "<leader>;", fzfLua.commands)
			vim.keymap.set("n", "<leader>i", function()
				fzfLua.files({ cwd = "~/.config/nvim" })
			end)
		end,
	},
	{ -- Git Client
		"tpope/vim-fugitive",
		config = function()
			vim.api.nvim_create_autocmd("FileType", {
				pattern = "fugitive",
				callback = function(ev)
					vim.keymap.set("n", "P", "<cmd>Git push<CR>", { buffer = ev.buf })
					vim.keymap.set("n", "p", "<cmd>Git pull --rebase<CR>", { buffer = ev.buf })
					vim.keymap.set("n", "gu", "<cmd>diffget //2<CR>")
					vim.keymap.set("n", "gh", "<cmd>diffget //3<CR>")
				end,
			})
		end,
	},
	{ -- LSP
		"neovim/nvim-lspconfig",
		dependencies = {
			"williamboman/mason.nvim",
			"williamboman/mason-lspconfig.nvim",
			{
				"j-hui/fidget.nvim",
				opts = {
					-- options
				},
			},
		},
		config = function()
			require("mason").setup()
			require("mason-lspconfig").setup({ ensure_installed = { "gopls" } })
			local lsp_servers = {
				gopls = {},
				ols = {},
				intelephense = {},
				lua_ls = {
					settings = {
						Lua = {
							telemetry = { enable = false },
							diagnostics = {
								globals = { "vim" },
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
					map("n", "gI", implementation, "[g]oto [i]mplementation")
					map("n", "gr", references, "[g]oto [r]eferences")
					map("n", "R", vim.lsp.buf.rename, "Rename")
					map("n", "K", function()
						vim.lsp.buf.hover({ border = border })
					end, "Hover")
					map("n", "C", vim.lsp.buf.code_action, "Code Actions")
					map("n", "<leader>f", vim.lsp.buf.format, "Format")
					map({ "n", "i" }, "<C-s>", function()
						vim.lsp.buf.signature_help({ border = border })
					end, "Signature Help")
					vim.diagnostic.config({
						enabled = false,
						virtual_text = false,
						float = { border = border },
					})
				end,
			})
		end,
	},
	{
		"folke/lazydev.nvim",
		ft = "lua", -- only load on lua files
		opts = {
			library = {
				-- See the configuration section for more details
				-- Load luvit types when the `vim.uv` word is found
				{ path = "${3rd}/luv/library", words = { "vim%.uv" } },
			},
		},
	},
	{
		"stevearc/oil.nvim",
		opts = {
			buf_options = {
				buflisted = true,
				bufhidden = "hide",
			},
		},
	},
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
		"folke/snacks.nvim",
		lazy = false,
		config = function()
			Snacks = require("snacks")
			Snacks.setup({
				bigfile = { enabled = true },
				explorer = { enabled = true },
				indent = {
					enabled = true,
					animate = { enabled = false },
					scope = { enabled = false },
					filter = function(buf)
						return vim.bo[buf].filetype == "yaml"
					end,
				},
				input = { enabled = true },
				picker = {
					enabled = FuzzyFinder == "snacks",
					layout = {
						preview = false,
						layout = {
							backdrop = false,
							width = 0.7,
							min_width = 80,
							height = 0.5,
							min_height = 3,
							box = "vertical",
							border = "rounded",
							title = "{title}",
							title_pos = "center",
							{ win = "input", height = 1, border = "bottom" },
							{ win = "list", border = "none" },
							{ win = "preview", title = "{preview}", height = 0.4, border = "top" },
						},
					},
				},
				notifier = { enabled = true },
				quickfile = { enabled = true },
				scope = { enabled = true },
			})
			-- Terminal
			vim.keymap.set({ "n", "t" }, "<c-j>", function()
				Snacks.terminal.toggle()
			end, {})

			vim.keymap.set("n", "<leader><leader>", function()
				Snacks.picker.files({})
			end, {})

			vim.keymap.set("n", "<leader>i", function()
				Snacks.picker.files({
					prompt = "Neovim configuration",
					cwd = vim.fn.stdpath("config"),
					preview = "none",
				})
			end, {})

			vim.keymap.set("n", "<leader>sd", function()
				Snacks.picker.files({ cwd = "~/.dotfiles" })
			end, {})

			vim.keymap.set("n", "<C-p>", function()
				Snacks.picker.git_files({})
			end, {})

			vim.keymap.set("n", "??", function()
				Snacks.picker.grep({ layout = "default" })
			end, {})

			vim.keymap.set("n", "<leader>o", function()
				Snacks.picker.lsp_symbols()
			end, {})

			vim.keymap.set("n", "<leader>O", function()
				Snacks.picker.lsp_workspace_symbols()
			end, {})

			vim.keymap.set("n", "<leader>h", function()
				Snacks.picker.help()
			end, {})

			vim.keymap.set("n", "<leader>b", function()
				Snacks.picker.buffers()
			end, {})

			vim.keymap.set("n", "<leader>d", function()
				Snacks.picker.diagnostics_buffer()
			end, {})

			vim.keymap.set("n", "<leader>D", function()
				Snacks.picker.diagnostics()
			end, {})

			vim.keymap.set("n", "<leader>;", function()
				Snacks.picker.command_history()
			end, {})

			vim.keymap.set("n", "<leader>sn", function()
				Snacks.picker.notifications()
			end, {})

			vim.keymap.set("n", "<leader>sc", function()
				Snacks.picker.commands()
			end, {})
			vim.keymap.set("n", "<leader>gb", function()
				Snacks.picker.git_branches()
			end, {})
			vim.keymap.set("n", "<leader>gl", function()
				Snacks.picker.git_log()
			end, {})
			vim.keymap.set("n", "<leader>gL", function()
				Snacks.picker.git_log_line()
			end, {})
			vim.keymap.set("n", "<leader>gs", function()
				Snacks.picker.git_status()
			end, {})
			vim.keymap.set("n", "<leader>gS", function()
				Snacks.picker.git_stash()
			end, {})
			vim.keymap.set("n", "<leader>gd", function()
				Snacks.picker.git_diff()
			end, {})
			vim.keymap.set("n", "<leader>gf", function()
				Snacks.picker.git_log_file()
			end, {})

			vim.keymap.set("n", "<leader>e", function()
				Snacks.explorer()
			end, {})

			-- Zen
			vim.keymap.set("n", "Z", function()
				Snacks.zen()
			end, {})
		end,
	},
	{
		"nvim-telescope/telescope.nvim",
		enabled = FuzzyFinder == "telescope",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope-ui-select.nvim",
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
		},

		config = function()
			require("telescope").setup({
				defaults = {
					layout_config = {
						width = 0.95, -- 90% of the editor width
						height = 0.85, -- 80% of the editor height
						preview_width = 0.6, -- Adjust preview width
						prompt_position = "top",
					},
					sorting_strategy = "ascending", -- Ensures results are listed from top to bottom
					file_ignore_patterns = {

						"node_modules",
						"vendor",
					},
				},
			})
			require("telescope").load_extension("ui-select")

			vim.keymap.set("n", "<leader>ff", function()
				require("telescope.builtin").find_files({ previewer = false })
			end)
			vim.keymap.set("n", "<leader><leader>", function()
				require("telescope.builtin").find_files({ previewer = false })
			end)

			vim.keymap.set("n", "<leader>i", function()
				require("telescope.builtin").find_files({
					prompt_title = "Neovim Config",
					cwd = vim.fn.stdpath("config"),
					previewer = false,
				})
			end)

			vim.keymap.set("n", "<c-p>", function()
				require("telescope.builtin").git_files({ previewer = false })
			end)

			vim.keymap.set("n", "??", function()
				require("telescope.builtin").live_grep({})
			end)

			vim.keymap.set("n", "<leader>o", function()
				require("telescope.builtin").lsp_document_symbols({
					previewer = false,
				})
			end)

			vim.keymap.set("n", "<leader>O", function()
				require("telescope.builtin").lsp_dynamic_workspace_symbols({
					previewer = false,
				})
			end)

			vim.keymap.set("n", "<leader>h", function()
				require("telescope.builtin").help_tags({})
			end)
			vim.keymap.set("n", "<leader>b", function()
				require("telescope.builtin").buffers({})
			end)
		end,
	},
	{
		"hrsh7th/nvim-cmp",
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
		},
		config = function()
			local cmp_select = { behavior = require("cmp").SelectBehavior.Select }
			local cmp = require("cmp")
			cmp.setup({
				preselect = require("cmp.types").cmp.PreselectMode.None,
				snippet = {
					expand = function(args)
						vim.snippet.expand(args.body)
					end,
				},
				mapping = cmp.mapping.preset.insert({
					["<C-p>"] = cmp.mapping.select_prev_item(cmp_select),
					["<C-n>"] = cmp.mapping.select_next_item(cmp_select),
					["<C-y>"] = cmp.mapping.confirm({ select = true }),
					["<CR>"] = cmp.mapping.confirm({ select = false }),
					["<C-Space>"] = cmp.mapping.complete(),
				}),
				sources = {
					{ name = "nvim_lsp" },
					{ name = "buffer" },
					{
						name = "lazydev",
						group_index = 0, -- set group index to 0 to skip loading LuaLS completions
					},
				},
			})
		end,
	},
	{
		"saghen/blink.cmp",
		dependencies = {},
		version = "*",
		enabled = false,
		opts = {
			keymap = { preset = "enter" },
			appearance = {
				nerd_font_variant = "mono",
			},
			completion = {
				documentation = {
					auto_show = true,
				},
			},
			sources = {
				default = { "lazydev", "lsp", "path", "snippets", "buffer" },
				providers = {
					lazydev = {
						name = "LazyDev",
						module = "lazydev.integrations.blink",
						score_offset = 100,
					},
				},
			},
			fuzzy = { implementation = "prefer_rust_with_warning" },
		},
		opts_extend = { "sources.default" },
	},
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		enabled = false,
		opts = {},
	},
	{
		"nvim-treesitter/nvim-treesitter",
		dependencies = {
			{ "folke/ts-comments.nvim", opts = {} },
		},
		config = function()
			require("nvim-treesitter.configs").setup({
				ensure_installed = { "lua", "go", "gomod", "markdown", "php", "c", "cpp", "odin" },
				highlight = { enable = true },
			})
		end,
	},
	{
		"tjdevries/colorbuddy.nvim",
	},
	{
		"folke/tokyonight.nvim",

		opts = {
			style = "moon",
			transparent = transparent,
		},
	},
	{
		"rose-pine/neovim",
		name = "rose-pine",
		opts = {
			dark_variant = "moon",
			styles = {
				italic = false,
				transparency = transparent,
			},
		},
	},
	{
		"ellisonleao/gruvbox.nvim",
		opts = {
			contrast = "hard",
			transparent_mode = transparent,
		},
	},

	{ "catppuccin/nvim", name = "catppuccin", opts = { transparent_background = transparent } },
	{ "navarasu/onedark.nvim", opts = { style = "dark", transparent = transparent } },
}, {
	change_detection = { notify = false },
})

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
vim.opt.cursorline = false
vim.opt.updatetime = 250
vim.opt.termsync = false
vim.opt.number = true -- Line numbers
vim.opt.mouse = "a"
vim.opt.clipboard = "unnamedplus" -- Clipboard
vim.opt.ignorecase = true -- Case-insensitive searching UNLESS \C or capital in search
vim.opt.smartcase = true
vim.opt.completeopt = { "menu", "noinsert" }
vim.opt.inccommand = "" -- Preview all substitutions(replacements).
vim.opt.scrolloff = 10 -- Minimal number of screen lines to keep above and below the cursor.
vim.opt.laststatus = 3 -- Global statusline
vim.opt.cursorline = false
vim.keymap.set("n", "Y", "y$", { desc = "Copy whole line" })
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
vim.keymap.set("n", "{", "<cmd>cprev<CR>") -- Quick fix list
vim.keymap.set("n", "}", "<cmd>cnext<CR>") -- Quickfix list
vim.keymap.set("i", "<C-Space>", "<C-x><C-o>")
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
vim.keymap.set("n", "<M-Left>", "<c-w>5>")
vim.keymap.set("n", "<M-Right>", "<c-w>5<")
vim.keymap.set("n", "<M-Up>", "<C-W>+")
vim.keymap.set("n", "<M-Down>", "<C-W>-")
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set("t", "<C-w><C-w>", function()
	vim.cmd([[ wincmd w ]])
end)
vim.keymap.set("n", "<leader>l", vim.diagnostic.open_float, { desc = "Diagnostics: Open float window" })
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Set Local list" })
vim.keymap.set("n", "<leader>g", "<cmd>LazyGit<CR>", { desc = "Lazy Git" })
vim.keymap.set({ "n", "i", "t" }, "<C-h>", "<cmd>tabprev<CR>", {})
vim.keymap.set({ "n", "i", "t" }, "<C-l>", "<cmd>tabnext<CR>", {})

vim.cmd([[ command! W :w ]])

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
if vim.fn.has("wsl") == 1 then
	vim.g.clipboard = {
		name = "WslClipboard",
		copy = {
			["+"] = "clip.exe",
			["*"] = "clip.exe",
		},
		paste = {
			["+"] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
			["*"] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
		},
		cache_enabled = 0,
	}
end
vim.api.nvim_create_autocmd("TextYankPost", {
	group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
	callback = function()
		vim.highlight.on_yank()
	end,
})
local floating_term = { win = -1, buf = -1 }

local function toggle_floating_terminal()
	if vim.api.nvim_buf_is_valid(floating_term.buf) and vim.api.nvim_win_is_valid(floating_term.win) then
		vim.api.nvim_win_hide(floating_term.win)
		return
	end

	if not vim.api.nvim_buf_is_valid(floating_term.buf) then
		print("creating floating term buffer")
		floating_term.buf = vim.api.nvim_create_buf(false, true)
	end

	local width = math.floor(vim.o.columns * 0.8)
	local height = math.floor(vim.o.lines * 0.8)
	local row = math.floor((vim.o.lines - height) / 2)
	local col = math.floor((vim.o.columns - width) / 2)

	local win = vim.api.nvim_open_win(floating_term.buf, true, {
		relative = "editor",
		width = width,
		height = height,
		row = row,
		col = col,
		style = "minimal",
		border = "rounded",
	})

	if vim.api.nvim_get_option_value("buftype", { buf = floating_term.buf }) ~= "terminal" then
		vim.cmd.term()
	end

	vim.cmd.startinsert()

	floating_term = { buf = floating_term.buf, win = win }
end

local bottom_terminal = { win = -1, buf = -1 }

local function toggle_bottom_terminal()
	if vim.api.nvim_buf_is_valid(bottom_terminal.buf) and vim.api.nvim_win_is_valid(bottom_terminal.win) then
		vim.api.nvim_win_hide(bottom_terminal.win)
		return
	end

	if not vim.api.nvim_buf_is_valid(bottom_terminal.buf) then
		bottom_terminal.buf = vim.api.nvim_create_buf(false, true)
	end

	local width = vim.o.columns
	local height = math.floor(vim.o.lines * 0.3)

	local win = vim.api.nvim_open_win(bottom_terminal.buf, true, {
		split = "below",
		width = width,
		height = height,
	})

	if vim.api.nvim_get_option_value("buftype", { buf = bottom_terminal.buf }) ~= "terminal" then
		vim.cmd.term()
	end

	vim.cmd.startinsert()

	bottom_terminal = { buf = bottom_terminal.buf, win = win }
end

local tab_terminal_state = { last_tab = -1 }

local function toggle_terminal_tab()
	local current_win = vim.api.nvim_get_current_win()
	if vim.wo[current_win].winbar == "Terminal" then
		vim.api.nvim_set_current_tabpage(tab_terminal_state.last_tab)
		return
	end
	for _, tab_id in ipairs(vim.api.nvim_list_tabpages()) do
		local win_id = vim.api.nvim_tabpage_get_win(tab_id)
		local buf_id = vim.api.nvim_win_get_buf(win_id)
		if vim.wo[win_id].winbar == "Terminal" and vim.bo[buf_id].buftype == "terminal" then
			tab_terminal_state.last_tab = vim.api.nvim_get_current_tabpage()
			vim.api.nvim_set_current_tabpage(tab_id)
			vim.cmd.startinsert()
			return
		end
	end

	tab_terminal_state.last_tab = vim.api.nvim_get_current_tabpage()
	vim.cmd.tabnew()
	local win_id = vim.api.nvim_get_current_win()
	vim.wo[win_id].winbar = "Terminal"
	vim.cmd.term()
	vim.cmd.startinsert()
end

vim.keymap.set({ "n", "t" }, "<c-j>", toggle_bottom_terminal)
vim.cmd.colorscheme(os.getenv("NVIM_COLORSCHEME") or "rose-pine")
