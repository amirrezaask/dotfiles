local M = {}

function M.bottom_terminal(height_pct)
  height_pct = height_pct or 30
  local display_height = vim.api.nvim_get_option "lines"
  local terminal_height = math.ceil(display_height * (height_pct / 100))

  vim.cmd(string.format("%dnew | term", terminal_height))
end

function M.new_tab_terminal()
  vim.cmd [[ tabnew | term ]]
end

return M
