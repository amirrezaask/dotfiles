local floating = require"amirrezaask.floating"
local M = {}

function M:new(prompt, callback)
  local buf, win = floating:new {
    width_pct = 60,
    height = 1,
  }
  vim.api.nvim_buf_set_option(buf, 'buftype', 'prompt')
  vim.fn.prompt_setprompt(buf, prompt)
  vim.c.startinsert()
  vim.fn.prompt_setcallback(buf, function(text)
    vim.api.nvim_win_close(win, true)
    callback(text)
  end)

end

return M


