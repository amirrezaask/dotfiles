local floating = {}
floating.__index = floating
floating.__call = floating.new

local function setup_buf(buf)
  vim.api.nvim_buf_set_option(buf, 'bufhidden', 'wipe')
  vim.api.nvim_buf_set_option(buf, 'buftype', 'nofile')
  vim.api.nvim_buf_set_keymap(buf, 'n', 'q', '<cmd>bd!<cr>', {})
end

local center = function(win_height, win_width)
  local width = vim.api.nvim_get_option('columns')
  local height = vim.api.nvim_get_option('lines')
  local row = math.ceil((height - win_height) / 2 - 1)
  local col = math.ceil((width - win_width) / 2)
  return row, col
end

local function job_to_buf(cmd, buf)
  vim.fn.jobstart(cmd, {
    on_exit = function(_, code, _)
      print('process exited with code ' .. code)
    end,
    on_stdout = function(_, data, _)
      vim.schedule(function() 
        local count = vim.api.nvim_buf_line_count(buf)
        vim.api.nvim_buf_set_lines(buf, count, -1, false, data)
      end)
    end,
    on_stderr = function(_, data, _)
      vim.schedule(function() 
        local count = vim.api.nvim_buf_line_count(buf)
        vim.api.nvim_buf_set_lines(buf, count, -1, false, data)
      end)
    end
  })

end

function floating:new(opts)
  opts = opts or {}
  opts.width_pct = opts.width_pct or 90
  opts.height_pct = opts.height_pct or 60

  local win_width = math.ceil(vim.api.nvim_get_option('columns') * opts.width_pct / 100)
  local win_height = math.ceil(vim.api.nvim_get_option('lines') * opts.height_pct / 100)
  local row, col = center(win_height, win_width)

  local buf = vim.api.nvim_create_buf(true, true)
  setup_buf(buf)
  local win = vim.api.nvim_open_win(buf, true, {
    relative = 'editor',
    width = win_width,
    height = win_height,
    row = row,
    col = col,
    border = 'single',
    style = 'minimal'
  })
  vim.api.nvim_win_set_option(win, 'winhl', 'Normal:Normal')
  return buf, win
end

function floating:vnew(command)
  vim.cmd [[ vnew ]]
  local buf = vim.api.nvim_get_current_buf()
  setup_buf(buf)
  job_to_buf(command or vim.fn.input('cmd: '), buf)
end

function floating:command()
  local cmd = vim.fn.input('command: ')
  local buf, _ = floating:new({source=cmd})
  job_to_buf(cmd, buf)
end
return floating
