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

	if vim.api.nvim_buf_get_option(floating_term.buf, "buftype") ~= "terminal" then
		print("starting terminal")
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

	if vim.api.nvim_buf_get_option(bottom_terminal.buf, "buftype") ~= "terminal" then
		vim.cmd.term()
	end

	vim.cmd.startinsert()

	bottom_terminal = { buf = bottom_terminal.buf, win = win }
end

vim.keymap.set({ "n", "t" }, "<c-j>", toggle_bottom_terminal)
