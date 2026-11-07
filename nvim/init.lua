-- Tiny settings, big opinions.
-- Somewhere, a plugin is taking notes.
-- This line has seen things.
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
	"https://github.com/nvim-treesitter/nvim-treesitter",
	"https://github.com/mason-org/mason.nvim",
	"https://github.com/folke/snacks.nvim",
})

-- Colors come from the terminal. The 16 ANSI slots + fg/bg are queried over OSC 4/10/11
-- and every other shade (cursorline, visual, popup, diff, diagnostics, dimmed text) is
-- blended from them, so nvim follows the terminal theme but still has a full palette.
-- Until the terminal answers (or if it never does) the cterm-only `vim` scheme is used.
vim.o.termguicolors = false
vim.cmd.colorscheme("vim")

local term = {}

local function rgb(hex)
	return { tonumber(hex:sub(1, 2), 16), tonumber(hex:sub(3, 4), 16), tonumber(hex:sub(5, 6), 16) }
end

local function mix(a, b, t)
	local out = "#"
	for i = 1, 3 do
		out = out .. string.format("%02x", math.floor(a[i] + (b[i] - a[i]) * t + 0.5))
	end
	return out
end

local applying = false
local function apply_terminal_palette()
	local bg, fg = term.bg, term.fg
	if not (bg and fg) then
		return
	end
	local c = {}
	for i = 0, 15 do
		if not term[i] then
			return
		end
		c[i] = term[i]
	end
	local function hex(v)
		return mix(v, v, 0)
	end
	local red, green, yellow, blue, magenta, cyan = c[1], c[2], c[3], c[4], c[5], c[6]
	local orange = rgb(mix(red, yellow, 0.5):sub(2))
	local dim = mix(bg, fg, 0.45)
	local faint = mix(bg, fg, 0.3)
	local function tint(color, t)
		return mix(bg, color, t)
	end

	applying = true
	vim.o.termguicolors = true
	vim.cmd.colorscheme("vim")
	for i = 0, 15 do
		vim.g["terminal_color_" .. i] = hex(c[i])
	end
	local hl = {
		Normal = { fg = hex(fg) },
		NormalFloat = { fg = hex(fg), bg = mix(bg, fg, 0.05) },
		FloatBorder = { fg = faint, bg = mix(bg, fg, 0.05) },
		FloatTitle = { fg = hex(blue), bold = true },
		CursorLine = { bg = mix(bg, fg, 0.06) },
		ColorColumn = { bg = mix(bg, fg, 0.06) },
		CursorLineNr = { fg = hex(yellow), bold = true },
		LineNr = { fg = faint },
		SignColumn = {},
		EndOfBuffer = { fg = mix(bg, fg, 0.15) },
		WinSeparator = { fg = mix(bg, fg, 0.2) },
		Visual = { bg = tint(blue, 0.3) },
		Search = { bg = tint(yellow, 0.35) },
		CurSearch = { fg = hex(bg), bg = hex(yellow) },
		IncSearch = { fg = hex(bg), bg = hex(orange) },
		MatchParen = { bg = tint(magenta, 0.35), bold = true },
		Pmenu = { fg = hex(fg), bg = mix(bg, fg, 0.08) },
		PmenuSel = { bg = mix(bg, fg, 0.2), bold = true },
		PmenuSbar = { bg = mix(bg, fg, 0.12) },
		PmenuThumb = { bg = dim },
		StatusLine = { fg = hex(fg), bg = mix(bg, fg, 0.1) },
		StatusLineNC = { fg = dim, bg = mix(bg, fg, 0.05) },
		Folded = { fg = dim, bg = mix(bg, fg, 0.06) },
		NonText = { fg = mix(bg, fg, 0.2) },
		Whitespace = { fg = mix(bg, fg, 0.2) },
		Directory = { fg = hex(blue) },
		Title = { fg = hex(blue), bold = true },
		ErrorMsg = { fg = hex(red), bold = true },
		WarningMsg = { fg = hex(yellow) },
		MoreMsg = { fg = hex(green) },
		Question = { fg = hex(green) },

		Comment = { fg = dim, italic = true },
		Constant = { fg = hex(orange) },
		String = { fg = hex(green) },
		Character = { fg = hex(green) },
		Number = { fg = hex(orange) },
		Boolean = { fg = hex(orange) },
		Identifier = { fg = hex(fg) },
		Function = { fg = hex(blue) },
		Statement = { fg = hex(magenta) },
		Operator = { fg = hex(cyan) },
		PreProc = { fg = hex(magenta) },
		Type = { fg = hex(yellow) },
		Special = { fg = hex(cyan) },
		Delimiter = { fg = mix(bg, fg, 0.65) },
		Error = { fg = hex(red) },
		Todo = { fg = hex(bg), bg = hex(yellow), bold = true },
		["@variable"] = { fg = hex(fg) },
		["@variable.builtin"] = { fg = hex(red) },
		["@variable.parameter"] = { fg = mix(fg, red, 0.3) },
		["@property"] = { fg = hex(cyan) },
		["@constructor"] = { fg = hex(yellow) },
		["@tag"] = { fg = hex(red) },
		["@tag.attribute"] = { fg = hex(yellow) },
		["@markup.heading"] = { fg = hex(blue), bold = true },
		["@markup.link"] = { fg = hex(cyan), underline = true },

		DiffAdd = { bg = tint(green, 0.2) },
		DiffDelete = { bg = tint(red, 0.2) },
		DiffChange = { bg = tint(blue, 0.15) },
		DiffText = { bg = tint(blue, 0.35) },
		Added = { fg = hex(green) },
		Removed = { fg = hex(red) },
		Changed = { fg = hex(blue) },
	}
	for name, color in pairs({ Error = red, Warn = yellow, Info = blue, Hint = cyan, Ok = green }) do
		hl["Diagnostic" .. name] = { fg = hex(color) }
		hl["DiagnosticVirtualText" .. name] = { fg = hex(color), bg = tint(color, 0.12) }
		hl["DiagnosticUnderline" .. name] = { undercurl = true, sp = hex(color) }
	end
	for name, spec in pairs(hl) do
		vim.api.nvim_set_hl(0, name, spec)
	end
	-- keep the terminal's own background (transparency, blur, wallpaper) visible
	for _, group in ipairs({ "Normal", "NormalNC", "SignColumn", "EndOfBuffer", "LineNr", "CursorLineNr" }) do
		vim.cmd.highlight(group .. " guibg=NONE")
	end
	applying = false
end

local apply_timer = assert(vim.uv.new_timer())
local function apply_soon()
	-- 18 replies arrive back to back; apply once after they settle
	apply_timer:stop()
	apply_timer:start(
		30,
		0,
		vim.schedule_wrap(function()
			apply_terminal_palette()
		end)
	)
end

local function parse_color(r, g, b)
	local function chan(h)
		return tonumber(h, 16) / (16 ^ #h - 1) * 255
	end
	return { chan(r), chan(g), chan(b) }
end

vim.api.nvim_create_autocmd("TermResponse", {
	callback = function(ev)
		local seq = ev.data and ev.data.sequence or ""
		local n, r, g, b = seq:match("^\27%]4;(%d+);rgb:(%x+)/(%x+)/(%x+)")
		if n then
			term[tonumber(n)] = parse_color(r, g, b)
		else
			local k
			k, r, g, b = seq:match("^\27%]1([01]);rgb:(%x+)/(%x+)/(%x+)")
			if k then
				term[k == "0" and "fg" or "bg"] = parse_color(r, g, b)
			else
				return
			end
		end
		apply_soon()
	end,
})

local function query_terminal_palette()
	local q = { "\27]10;?\27\\", "\27]11;?\27\\" }
	for i = 0, 15 do
		q[#q + 1] = ("\27]4;%d;?\27\\"):format(i)
	end
	vim.api.nvim_ui_send(table.concat(q))
end
query_terminal_palette()
-- re-query when returning to nvim so a terminal theme switch is picked up
vim.api.nvim_create_autocmd("FocusGained", { callback = query_terminal_palette })
-- a :colorscheme change wipes our groups; re-apply them
vim.api.nvim_create_autocmd("ColorScheme", {
	callback = function()
		if vim.o.termguicolors and not applying then
			vim.schedule(apply_terminal_palette)
		end
	end,
})

local mason_bin = vim.fn.stdpath("data") .. "/mason/bin"
vim.env.PATH = mason_bin .. ":" .. (vim.env.PATH or "")
require("mason").setup({})

--------------------------------------------------------------------
-- find / grep / buffers / format / netrw
--------------------------------------------------------------------

local function executable(bin)
	return vim.fn.executable(bin) == 1
end

require("snacks").setup({
	picker = { enabled = true },
})

local js_fts = {
	astro = true,
	javascript = true,
	javascriptreact = true,
	typescript = true,
	typescriptreact = true,
	svelte = true,
}

local function has_markers(markers, filename)
	local start = filename ~= "" and vim.fs.dirname(filename) or vim.fn.getcwd()
	return vim.fs.find(markers, { path = start, upward = true, stop = vim.uv.os_homedir() })[1] ~= nil
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

vim.keymap.set("n", "<leader><leader>", function()
	Snacks.picker.files({ hidden = true, follow = true })
end, { desc = "Find Files" })
vim.keymap.set("n", "<C-p>", function()
	Snacks.picker.files({ hidden = true, follow = true })
end, { desc = "Find Files" })
vim.keymap.set("n", "<leader>f", function()
	Snacks.picker.files({ hidden = true, follow = true })
end, { desc = "Find Files" })
vim.keymap.set("n", "<leader>j", function()
	Snacks.picker.grep({ hidden = true })
end, { desc = "Grep" })
vim.keymap.set({ "n", "x" }, "<leader>J", function()
	Snacks.picker.grep_word({ hidden = true })
end, { desc = "Grep Word" })
vim.keymap.set("n", "<leader>k", function()
	Snacks.picker.buffers()
end, { desc = "Buffers" })
vim.keymap.set("n", "<leader>l", function()
	Snacks.picker.lines()
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
