local lua = {}

-- Force reload the module
function R(mod)
	package.loaded[mod] = nil
	return require(mod)
end

-- is the package loaded ?
function L(mod)
	return package.loaded[mod] ~= nil
end

-- Printer
function P(obj)
	print(vim.inspect(obj))
end

-- Eval line
function E()
	local filetype = vim.api.nvim_buf_get_option(0, "filetype")
	local line = vim.fn.getline(".")
	if filetype == "lua" then
		vim.cmd(string.format([[ lua %s ]], line))
	end
end

function lua.format(opts)
	opts = opts or {}
	opts.config = opts.config or os.getenv("HOME") .. "/.stylua.toml"
	print(opts.config)
	local filename = vim.api.nvim_buf_get_name(0)
	print(filename)
	vim.cmd(string.format([[ ! stylua --config-path %s %s ]], opts.config, filename))
end

require("nvim").mode_map({
	n = {
		["<Space>x"] = "<cmd>lua EVAL()<CR>",
		["<Space>X"] = "<cmd>luafile %<CR>",
	},
})

return lua
