local function toggle_bottom_terminal() -- Toggle terminal at the bottom of the screen, why install a plugin for this ?
  -- We have a valid buffer showing the terminal
  if not vim.g.bottom_terminal_buffer or not vim.api.nvim_buf_is_valid(vim.g.bottom_terminal_buffer) then
    vim.g.bottom_terminal_buffer = vim.api.nvim_create_buf(false, true)
  end
  for _, win in ipairs(vim.api.nvim_list_wins()) do -- Toggle if a window showing terminal is open
    local win_buf = vim.api.nvim_win_get_buf(win)
    if win_buf == vim.g.bottom_terminal_buffer then
      vim.api.nvim_win_hide(win)
      return
    end
  end

  local win = vim.api.nvim_open_win(vim.g.bottom_terminal_buffer, true, {
    win = -1,
    split = "below",
    height = math.floor(vim.o.lines * 0.45),
    width = vim.o.columns,
  })
  vim.wo[win].winfixheight = true

  if vim.bo[vim.g.bottom_terminal_buffer].buftype ~= "terminal" then
    vim.cmd.term()
    vim.g.bottom_terminal_buffer = vim.api.nvim_get_current_buf()
  end

  vim.cmd.startinsert()
end

vim.keymap.set({ "n", "t" }, "<C-s>", toggle_bottom_terminal, { desc = "Toggle bottom terminal" })
