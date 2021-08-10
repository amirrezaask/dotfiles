local floating = {}
floating.__index = floating
floating.__call = floating.new

local function setup_buf(buf)
  vim.api.nvim_buf_set_option(buf, "bufhidden", "wipe")
  vim.api.nvim_buf_set_option(buf, "buftype", "nofile")
  vim.api.nvim_buf_set_keymap(buf, "n", "q", ":bd!<cr>", {})
end

local center = function(win_height, win_width)
  local width = vim.api.nvim_get_option "columns"
  local height = vim.api.nvim_get_option "lines"
  local row = math.ceil((height - win_height) / 2 - 1)
  local col = math.ceil((width - win_width) / 2)
  return row, col
end

function floating:new(opts)
  opts = opts or {}
  opts.width_pct = opts.width_pct or 90
  opts.height_pct = opts.height_pct or 60
  opts.border = opts.border or true

  local win_width = math.ceil(vim.api.nvim_get_option "columns" * opts.width_pct / 100)
  win_width = opts.width or win_width
  local win_height = math.ceil(vim.api.nvim_get_option "lines" * opts.height_pct / 100)
  win_height = opts.height or win_height
  local row, col = center(win_height, win_width)

  local border_buf
  local border_win
  if opts.border then
    local border_win_opts = {
      style = "minimal",
      relative = "editor",
      width = win_width + 2,
      height = win_height + 2,
      row = row - 1,
      col = col - 1,
    }
    border_buf = vim.api.nvim_create_buf(false, true)
    border_win = vim.api.nvim_open_win(border_buf, true, border_win_opts)
    local top_line = "┌" .. string.rep("─", win_width) .. "┐"
    local middle_line = "│" .. string.rep(" ", win_width) .. "│"
    local bottom_line = "└" .. string.rep("─", win_width) .. "┘"

    local border_lines = { top_line }

    for _ = 1, win_height do
      table.insert(border_lines, middle_line)
    end

    table.insert(border_lines, bottom_line)
    for i = 0, win_height - 1 do
      vim.api.nvim_buf_add_highlight(border_buf, 0, "PopupWindowBorder", i, 0, -1)
    end
    vim.api.nvim_buf_set_lines(border_buf, 0, -1, false, border_lines)
    vim.api.nvim_win_set_option(border_win, "wrap", false)
    vim.api.nvim_win_set_option(border_win, "number", false)
    vim.api.nvim_win_set_option(border_win, "relativenumber", false)
    vim.api.nvim_win_set_option(border_win, "cursorline", false)
    vim.api.nvim_win_set_option(border_win, "signcolumn", "no")
    vim.api.nvim_win_set_option(border_win, "winhl", "Normal:FuzzyBorderNormal")
  end
  local buf = vim.api.nvim_create_buf(true, true)
  setup_buf(buf)
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = win_width,
    height = win_height,
    row = row,
    col = col,
    -- TODO: re visit this when borders are customizable in neovim
    border = "none",
    style = "minimal",
  })
  vim.autocmd {
    "BufEnter",
    string.format("<buffer=%s>", border_buf),
    function()
      vim.api.nvim_set_current_win(win)
    end,
  }
  vim.autocmd {
    "BufLeave",
    string.format("<buffer=%s>", buf),
    function()
      if opts.border then
        pcall(vim.api.nvim_win_close, border_win, { force = true })
      end
    end,
  }
  vim.autocmd {
    "TermClose",
    "*",
    function()
      if opts.border then
        pcall(vim.api.nvim_win_close, border_win, { force = true })
      end
      vim.api.nvim_win_close(win, { force = true })
    end,
  }

  vim.api.nvim_win_set_option(win, "winhl", "Normal:Normal")
  return buf, win
end

local function cmd_result_to_buf(cmd, buf, opts)
  opts = opts or {}
  opts.on_exit = function(_, code, _)
    print("process exited with code " .. code)
  end
  opts.on_stdout = function(_, data, _)
    vim.schedule(function()
      local count = vim.api.nvim_buf_line_count(buf)
      vim.api.nvim_buf_set_lines(buf, count, -1, false, data)
    end)
  end
  opts.on_stderr = function(_, data, _)
    vim.schedule(function()
      local count = vim.api.nvim_buf_line_count(buf)
      vim.api.nvim_buf_set_lines(buf, count, -1, false, data)
    end)
  end
  vim.fn.jobstart(cmd, opts)
end

function floating:vnew(command)
  vim.c.vnew()
  local buf = vim.api.nvim_get_current_buf()
  setup_buf(buf)
  cmd_result_to_buf(command or vim.fn.input "cmd: ", buf)
end

function floating:command(cmd, opts)
  opts = opts or {}
  cmd = cmd or vim.fn.input "command: "
  local buf, _, _, _ = floating:new(opts)
  cmd_result_to_buf(cmd, buf, opts.jobstart or {})
end

function floating:prompt(prompt, callback)
  local origin_win = vim.api.nvim_get_current_win()
  local buf, win = floating:new {
    width_pct = 60,
    height = 1,
  }
  vim.api.nvim_buf_set_option(buf, "buftype", "prompt")
  vim.api.nvim_buf_set_keymap(buf, "n", "q", string.format("<cmd>call nvim_win_close(%s, 1)<CR>", win), {})
  vim.api.nvim_buf_set_keymap(buf, "n", "<C-C>", string.format("<cmd>call nvim_win_close(%s, 1)<CR>", win), {})
  vim.api.nvim_buf_set_keymap(buf, "i", "<C-c>", string.format("<cmd>call nvim_win_close(%s, 1)<CR>", win), {})
  vim.fn.prompt_setprompt(buf, prompt)
  vim.c.startinsert()
  vim.fn.prompt_setcallback(buf, function(text)
    vim.api.nvim_win_close(win, true)
    vim.api.nvim_set_current_win(origin_win)
    callback(text)
  end)
end

return floating
