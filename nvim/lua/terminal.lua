local M = {}

---@class TerminalOpts
---@field cmd? string
---@field location? string
---@field buf? number

---@param opts TerminalOpts
--- If you pass in a buffer, it will be used instead of creating a new one
function M.get(opts)
  opts = opts or {}
  opts.cmd = opts.cmd or nil
  opts.location = opts.location or "float"
  opts.buf = opts.buf or -1

  if not vim.api.nvim_buf_is_valid(opts.buf) then
    opts.buf = vim.api.nvim_create_buf(false, true)
  end

  local win
  if opts.location == "float" then
    local height = math.floor(vim.o.lines * 0.8)
    local width = math.floor(vim.o.columns * 0.8)

    local row = math.floor((vim.o.lines - height) / 2)
    local col = math.floor((vim.o.columns - width) / 2)
    win = vim.api.nvim_open_win(opts.buf, true, {
      relative = "editor",
      width = width,
      height = height,
      row = row,
      col = col,
      style = "minimal",
      border = "rounded",
    })
  elseif opts.location == "bottom" then
    local height = math.floor(vim.o.lines * 0.3)
    win = vim.api.nvim_open_win(opts.buf, true, {
      split = "below",
      height = height,
    })
  else
    vim.error("Invalid location for terminal")
    return
  end

  if opts.cmd == "" then
    opts.cmd = nil
  end

  if vim.bo[opts.buf].buftype ~= "terminal" then
    vim.cmd.term(opts.cmd)
  end

  local close = function()
    vim.api.nvim_buf_delete(opts.buf, { force = true })
  end

  vim.keymap.set("n", "q", close, { buffer = opts.buf })

  vim.cmd.startinsert()
  return { win = win, buf = opts.buf }
end

local terminal_state = { buf = -1, win = -1 }

---@function returns a function that toggles terminal in specified location
---@param terminal_location string float|bottom
---@returns function fun() Toggles Terminal in specified location
function M.toggle(terminal_location)
  return function()
    terminal_location = terminal_location or "float"
    if vim.api.nvim_buf_is_valid(terminal_state.buf) and vim.api.nvim_win_is_valid(terminal_state.win) then
      vim.api.nvim_win_hide(terminal_state.win)
      return
    end

    if not vim.api.nvim_buf_is_valid(terminal_state.buf) then
      terminal_state.buf = vim.api.nvim_create_buf(false, true)
    end

    local terminal = M.get({ location = terminal_location, buf = terminal_state.buf })
    if terminal == nil then
      return
    end
    terminal_state.buf = terminal.buf
    terminal_state.win = terminal.win
  end
end

return M
