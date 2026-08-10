vim.o.number = true
vim.o.relativenumber = true
vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.smartindent = true
vim.o.wrap = true
vim.o.swapfile = false
vim.o.backup = false
vim.o.signcolumn = "yes"
vim.o.undofile = true
vim.o.incsearch = true
vim.o.wildoptions = vim.o.wildoptions .. ",fuzzy"
vim.o.wildmode = "noselect:lastused,full"
vim.o.clipboard = "unnamedplus"
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.cursorline = true
vim.o.winborder = "rounded"
vim.o.completeopt = "menuone,noselect,popup"
vim.o.grepprg = "rg --vimgrep --smart-case --hidden"
vim.o.grepformat = "%f:%l:%c:%m"

vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.g.netrw_liststyle = 3
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.g.netrw_browse_split = 0
vim.g.netrw_altfile = 1

vim.keymap.set("i", "jk", "<esc>")
vim.keymap.set("i", "kj", "<esc>")
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("i", "<CR>", function()
	if vim.fn.pumvisible() == 1 then
		local info = vim.fn.complete_info({ "selected" })
		if info.selected == -1 then
			return "<C-n><C-y>"
		end
		return "<C-y>"
	end
	return "<CR>"
end, { expr = true })
vim.keymap.set("v", "<", "<gv", { desc = "Indent Left" })
vim.keymap.set("v", ">", ">gv", { desc = "Indent Right" })
vim.keymap.set("v", "p", '"_dP', { desc = "Paste (no yank)" })
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("n", "<leader>i", ":edit $MYVIMRC<CR>")
vim.keymap.set("n", "<C-q>", function()
	if vim.fn.getqflist({ winid = 0 }).winid ~= 0 then
		vim.cmd.cclose()
	else
		vim.cmd.copen()
	end
end)
vim.keymap.set("n", "<leader>q", function()
	vim.diagnostic.setloclist({ open = true })
end)
vim.keymap.set("n", "<CR>", function()
	if vim.v.hlsearch == 1 then
		vim.cmd.nohl()
		return ""
	end
	return vim.keycode("<CR>")
end, { expr = true })

vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter", "CursorHold" }, {
	callback = function()
		vim.cmd("checktime")
	end,
})
vim.api.nvim_create_autocmd("FileType", {
	callback = function(args)
		pcall(vim.treesitter.start, args.buf)
	end,
})
vim.api.nvim_create_autocmd("TextYankPost", {
	callback = function()
		vim.hl.hl_op({ higroup = "Visual", timeout = 150 })
	end,
})
vim.api.nvim_create_autocmd("BufEnter", {
	callback = function(args)
		if vim.bo[args.buf].buftype == "prompt" then
			vim.bo[args.buf].autocomplete = false
		end
	end,
})
vim.api.nvim_create_autocmd("DiagnosticChanged", {
	callback = function()
		vim.diagnostic.setqflist({ open = false })
	end,
})
vim.api.nvim_create_autocmd("VimResized", { command = "wincmd =" })
vim.api.nvim_create_autocmd("BufReadPost", {
	callback = function(args)
		local mark = vim.api.nvim_buf_get_mark(args.buf, '"')
		local line_count = vim.api.nvim_buf_line_count(args.buf)
		if mark[1] > 0 and mark[1] <= line_count then
			vim.api.nvim_win_set_cursor(0, mark)
			vim.schedule(function()
				vim.cmd("normal! zz")
			end)
		end
	end,
})

vim.pack.add({
	{ src = "https://github.com/catppuccin/nvim", name = "catppuccin" },
	"https://github.com/nvim-treesitter/nvim-treesitter",
	"https://github.com/mason-org/mason.nvim",
})
require("catppuccin").setup({ flavour = "mocha" })
vim.cmd.colorscheme("catppuccin-macchiato")

local mason_bin = vim.fn.stdpath("data") .. "/mason/bin"
vim.env.PATH = mason_bin .. ":" .. (vim.env.PATH or "")
require("mason").setup({})

--------------------------------------------------------------------
-- find / grep / buffers / format / netrw
--------------------------------------------------------------------

local file_cache, file_cache_cwd = {}, nil

local function executable(bin)
	return vim.fn.executable(bin) == 1
end

local function list_files()
	local cwd = vim.fn.getcwd()
	if file_cache_cwd == cwd and #file_cache > 0 then
		return file_cache
	end
	local files
	if executable("fd") then
		files =
			vim.fn.systemlist({ "fd", "--type", "f", "--hidden", "--follow", "--exclude", ".git", "--color", "never" })
		if vim.v.shell_error ~= 0 then
			files = {}
		end
	elseif executable("rg") then
		files = vim.fn.systemlist({ "rg", "--files", "--hidden", "--follow", "--glob", "!.git", "--color", "never" })
		if vim.v.shell_error ~= 0 then
			files = {}
		end
	else
		files = vim.tbl_filter(function(f)
			return vim.fn.isdirectory(f) == 0
		end, vim.fn.glob("**/*", true, true))
	end
	file_cache, file_cache_cwd = files, cwd
	return file_cache
end

local function fuzzy_filter(files, pattern)
	if pattern == "" then
		return files
	end
	if executable("fzf") then
		local out = vim.fn.systemlist({ "fzf", "--filter=" .. pattern }, table.concat(files, "\n") .. "\n")
		if vim.v.shell_error == 0 or #out > 0 then
			return out
		end
	end
	return vim.fn.matchfuzzy(files, pattern)
end

function _G.native_find(cmdarg, _)
	return fuzzy_filter(list_files(), cmdarg or "")
end
vim.o.findfunc = "v:lua.native_find"

vim.api.nvim_create_autocmd({ "DirChanged", "CmdlineEnter" }, {
	callback = function(args)
		if args.event == "CmdlineEnter" and vim.fn.getcmdtype() ~= ":" then
			return
		end
		file_cache, file_cache_cwd = {}, nil
	end,
})

local function grep(pattern)
	if not pattern or pattern == "" then
		return
	end
	vim.cmd("silent grep! " .. vim.fn.fnameescape(pattern))
	vim.cmd("copen")
end

local function fuzzy_lines(pattern)
	if not pattern or pattern == "" then
		return
	end
	local bufnr = vim.api.nvim_get_current_buf()
	local candidates = {}
	for i, line in ipairs(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)) do
		candidates[#candidates + 1] = { text = line, lnum = i }
	end
	local items = {}
	for _, m in ipairs(vim.fn.matchfuzzy(candidates, pattern, { key = "text" })) do
		items[#items + 1] = { bufnr = bufnr, lnum = m.lnum, col = 1, text = m.text }
	end
	vim.fn.setloclist(0, items, " ")
	vim.fn.setloclist(0, {}, "a", { title = "Buffer lines: " .. pattern })
	vim.cmd("lopen")
end

local js_fts = {
	astro = true,
	javascript = true,
	javascriptreact = true,
	typescript = true,
	typescriptreact = true,
	svelte = true,
}

local function has_markers(markers, filename)
	return vim.fs.find(markers, { path = filename, upward = true, stop = vim.uv.os_homedir() })[1] ~= nil
end

local function format_cmdline(bufnr)
	local ft = vim.bo[bufnr].filetype
	local name = vim.api.nvim_buf_get_name(bufnr)
	if ft == "lua" and executable("stylua") then
		return { "stylua", "-" }
	end
	if ft == "go" and executable("goimports") then
		return { "goimports" }
	end
	if (ft == "json" or ft == "jsonc") and executable("jq") then
		return { "jq", "." }
	end
	if js_fts[ft] then
		if executable("oxfmt") and has_markers({ ".oxfmtrc.json", ".oxfmtrc.jsonc" }, name) then
			return { "oxfmt", "--stdin-filepath", name }
		end
		if executable("biome") and has_markers({ "biome.json", "biome.jsonc" }, name) then
			return { "biome", "format", "--stdin-file-path", name }
		end
		if
			executable("prettierd")
			and has_markers({
				".prettierrc",
				".prettierrc.json",
				".prettierrc.js",
				".prettierrc.cjs",
				".prettierrc.mjs",
				"prettier.config.js",
				"prettier.config.cjs",
				"prettier.config.mjs",
			}, name)
		then
			return { "prettierd", name }
		end
		if ft == "svelte" and executable("prettierd") then
			return { "prettierd", name }
		end
	end
	return nil
end

local function format_buf(bufnr)
	bufnr = bufnr or vim.api.nvim_get_current_buf()
	if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
		return
	end
	local cmd = format_cmdline(bufnr)
	if cmd then
		local output = vim.fn.system(cmd, table.concat(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false), "\n"))
		if vim.v.shell_error == 0 then
			local formatted = vim.split(output, "\n", { plain = true })
			if formatted[#formatted] == "" then
				table.remove(formatted)
			end
			vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, formatted)
		end
		return
	end
	for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
		if client:supports_method("textDocument/formatting") then
			vim.lsp.buf.format({ bufnr = bufnr, async = false })
			return
		end
	end
end

vim.api.nvim_create_autocmd("BufWritePre", {
	callback = function(args)
		format_buf(args.buf)
	end,
})

vim.keymap.set("n", "<leader><leader>", ":find ", { desc = "Find Files" })
vim.keymap.set("n", "<C-p>", ":find ", { desc = "Find Files" })
vim.keymap.set("n", "<leader>f", ":find ", { desc = "Find Files" })
vim.keymap.set("n", "<leader>j", function()
	vim.ui.input({ prompt = "Grep: " }, grep)
end, { desc = "Grep" })
vim.keymap.set("n", "<leader>J", function()
	grep(vim.fn.expand("<cword>"))
end, { desc = "Grep Word" })
vim.keymap.set("v", "<leader>J", function()
	local selection =
		table.concat(vim.fn.getregion(vim.fn.getpos("v"), vim.fn.getpos("."), { type = vim.fn.mode() }), "\n")
	vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "n", false)
	grep(vim.fn.trim(selection))
end, { desc = "Grep Word" })
vim.keymap.set("n", "<leader>k", ":buffer ", { desc = "Buffers" })
vim.keymap.set("n", "<leader>l", function()
	vim.ui.input({ prompt = "Lines: " }, fuzzy_lines)
end, { desc = "Buffer Lines" })
vim.keymap.set("n", "<leader>e", ":Lexplore<CR>", { desc = "File Tree" })

vim.api.nvim_create_user_command("Format", function()
	format_buf()
end, { desc = "Format current buffer" })
vim.api.nvim_create_user_command("Json", function()
	vim.bo.filetype = "json"
end, { desc = "Set buffer filetype to JSON" })

--------------------------------------------------------------------
-- LSP (configs inlined from nvim-lspconfig; no lspconfig plugin)
--------------------------------------------------------------------

vim.diagnostic.config({ virtual_text = true })

vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		local client = vim.lsp.get_client_by_id(args.data.client_id)
		if client and client:supports_method("textDocument/completion") then
			vim.lsp.completion.enable(true, client.id, args.buf, { autotrigger = true })
		end
		local buf = args.buf
		local map = function(mode, lhs, rhs, desc)
			vim.keymap.set(mode, lhs, rhs, { buffer = buf, desc = desc })
		end
		map("n", "gd", vim.lsp.buf.definition, "[g]oto [d]efinition")
		map("n", "grr", vim.lsp.buf.references, "[g]oto [r]eferences")
		map("n", "gri", vim.lsp.buf.implementation, "[g]oto [i]mplementations")
		map("n", "gO", vim.lsp.buf.document_symbol, "[g]oto symbol")
		map("n", "<leader>o", vim.lsp.buf.document_symbol, "[s]ymbols (outline)")
		map("n", "<leader>O", function()
			vim.ui.input({ prompt = "Workspace symbol: " }, function(query)
				if query then
					vim.lsp.buf.workspace_symbol(query)
				end
			end)
		end, "[s]ymbols [w]orkspace")
		map("n", "L", vim.diagnostic.open_float, "Open Floating Diagnostic")
		map("n", "C", vim.lsp.buf.code_action, "Code Actions")
	end,
})

-- TypeScript / JavaScript (ts_ls)
vim.lsp.config("ts_ls", {
	init_options = { hostInfo = "neovim" },
	cmd = { "typescript-language-server", "--stdio" },
	filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact" },
	root_dir = function(bufnr, on_dir)
		local root_markers = { "package-lock.json", "yarn.lock", "pnpm-lock.yaml", "bun.lockb", "bun.lock" }
		root_markers = vim.fn.has("nvim-0.11.3") == 1 and { root_markers, { ".git" } }
			or vim.list_extend(root_markers, { ".git" })
		local deno_root = vim.fs.root(bufnr, { "deno.json", "deno.jsonc" })
		local deno_lock_root = vim.fs.root(bufnr, { "deno.lock" })
		local project_root = vim.fs.root(bufnr, root_markers)
		if deno_lock_root and (not project_root or #deno_lock_root > #project_root) then
			return
		end
		if deno_root and (not project_root or #deno_root >= #project_root) then
			return
		end
		on_dir(project_root or vim.fn.getcwd())
	end,
})

-- Go (gopls)
vim.lsp.config("gopls", {
	cmd = { "gopls" },
	filetypes = { "go", "gomod", "gowork", "gotmpl" },
	root_markers = { "go.work", "go.mod", ".git" },
	settings = {
		gopls = {
			gofumpt = true,
			staticcheck = true,
			hints = {
				assignVariableTypes = true,
				compositeLiteralFields = true,
				compositeLiteralTypes = true,
				constantValues = true,
				functionTypeParameters = true,
				parameterNames = true,
				rangeVariableTypes = true,
			},
		},
	},
})

-- Rust (rust_analyzer)
vim.lsp.config("rust_analyzer", {
	cmd = { "rust-analyzer" },
	filetypes = { "rust" },
	root_dir = function(bufnr, on_dir)
		local fname = vim.api.nvim_buf_get_name(bufnr)
		local cargo_crate_dir = vim.fs.root(fname, { "Cargo.toml" })
		if cargo_crate_dir == nil then
			on_dir(vim.fs.root(fname, { "rust-project.json", ".git" }))
			return
		end
		vim.system({
			"cargo",
			"metadata",
			"--no-deps",
			"--format-version",
			"1",
			"--manifest-path",
			cargo_crate_dir .. "/Cargo.toml",
		}, { text = true }, function(output)
			local root = cargo_crate_dir
			if output.code == 0 and output.stdout then
				local ok, result = pcall(vim.json.decode, output.stdout)
				if ok and result.workspace_root then
					root = vim.fs.normalize(result.workspace_root)
				end
			end
			on_dir(root)
		end)
	end,
	settings = {
		["rust-analyzer"] = {
			cargo = { allFeatures = true },
			check = { command = "clippy" },
		},
	},
	before_init = function(init_params, config)
		if config.settings and config.settings["rust-analyzer"] then
			init_params.initializationOptions = config.settings["rust-analyzer"]
		end
	end,
})

-- Zig (zls)
vim.lsp.config("zls", {
	cmd = { "zls" },
	filetypes = { "zig", "zir" },
	root_markers = { "zls.json", "build.zig", ".git" },
	workspace_required = false,
})

-- Lua (for editing this config)
vim.lsp.config("lua_ls", {
	cmd = { "lua-language-server" },
	filetypes = { "lua" },
	root_markers = { ".luarc.json", ".luarc.jsonc", ".stylua.toml", "stylua.toml", ".git" },
	settings = {
		Lua = {
			runtime = { version = "LuaJIT" },
			diagnostics = { globals = { "vim" } },
			workspace = {
				checkThirdParty = false,
				library = { vim.env.VIMRUNTIME },
			},
		},
	},
})

vim.lsp.enable({ "ts_ls", "gopls", "rust_analyzer", "zls", "lua_ls" })
