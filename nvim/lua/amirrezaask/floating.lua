local floating = {}
floating.__index = floating
floating.__call = floating.new

local center = function(win_height, win_width)
  local width = vim.api.nvim_get_option('columns')
  local height = vim.api.nvim_get_option('lines')
  local row = math.ceil((height - win_height) / 2 - 1)
  local col = math.ceil((width - win_width) / 2)
  return row, col
end


function floating:new(opts)
  opts = opts or {}
  assert(opts.source, 'need to specify source for the floating window')
  opts.width_pct = opts.width_pct or 80
  opts.height_pct = opts.height_pct or 60

  local win_width = math.ceil(vim.api.nvim_get_option('columns') * opts.width_pct / 100)
  local win_height = math.ceil(vim.api.nvim_get_option('lines') * opts.height_pct / 100)
  local row, col = center(win_height, win_width)

  local buf = vim.api.nvim_create_buf(true, true)
  vim.api.nvim_buf_set_option(buf, 'bufhidden', 'wipe')
  vim.api.nvim_buf_set_option(buf, 'buftype', 'nofile')
  vim.api.nvim_buf_set_keymap(buf, 'n', 'q', '<cmd>bd!<cr>', {})

  local win = vim.api.nvim_open_win(buf, true, {
    relative = 'editor',
    width = win_width,
    height = win_height,
    row = row,
    col = col,
    border = 'single'
  })
  vim.api.nvim_win_set_option(win, 'winhl', 'Normal:Normal')
  if type(opts.source) == 'string' then
    vim.fn.jobstart(opts.source, {
      on_exit = function(_, code, _)
        if code ~= 0 then
          print('process exited with code ' .. code)
        end
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
  elseif type(opts.source) == 'table' then
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, opts.source)
  end
end 

vim.cmd [[command! -nargs=1 Floating lua require'amirrezaask.floating':new({source=<f-args>})]]

return floating
