-- Wrapper for FZF
local api = vim.api
local location = {
  center = function(win_height, win_width)
    local width = vim.api.nvim_get_option("columns")
    local height = vim.api.nvim_get_option("lines")
    local row = math.ceil((height - win_height) / 2 - 1)
    local col = math.ceil((width - win_width) / 2)
    return row, col
  end,
  bottom_center = function(win_height, win_width)
    local width = vim.api.nvim_get_option("columns")
    local height = vim.api.nvim_get_option("lines")
    local row = math.ceil((height - win_height)) 
    local col = math.ceil((width - win_width) / 2 )  
    return row, col
  end
}

-- Create a floating buffer with given win_width and win_height in given row and col.
local function floating_buffer(win_width, win_height, loc)
  local row, col = loc(win_height, win_width)
  local opts = {
    style = "minimal",
    relative = "editor",
    width = win_width,
    height = win_height,
    row = row,
    col = col
  }
  local buf = api.nvim_create_buf(true, true)
  api.nvim_buf_set_option(buf, 'bufhidden', 'wipe')
  api.nvim_buf_set_option(buf, 'buftype','prompt')
  local border_opts = {
    style = "minimal",
    relative = "editor",
    width = win_width + 2,
    height = win_height + 2,
    row = row - 1,
    col = col - 1
  }
  local border_buf = api.nvim_create_buf(false, true)

  local top_line ='╭' .. string.rep('─', win_width) .. '╮'
  local middle_line = '│' .. string.rep(' ', win_width) .. '│'
  local bottom_line =  '╰' .. string.rep('─', win_width) .. '╯'

  local border_lines = {top_line}

  for i=1, win_height do
    table.insert(border_lines, middle_line)
  end

  table.insert(border_lines, bottom_line)
  for i=0,win_height-1 do
    api.nvim_buf_add_highlight(border_buf, 0, 'PopupWindowBorder', i, 0, -1)
  end

  api.nvim_buf_set_lines(border_buf, 0, -1, false, border_lines)
  local border_win = api.nvim_open_win(border_buf, true, border_opts)
  api.nvim_win_set_option(border_win, 'wrap', false)
  api.nvim_win_set_option(border_win, 'number', false)
  api.nvim_win_set_option(border_win, 'relativenumber', false)
  api.nvim_win_set_option(border_win, 'cursorline', false)
  api.nvim_win_set_option(border_win, 'signcolumn', 'no')
  local win = api.nvim_open_win(buf, true, opts)
  return buf, win, function()
    vim.api.nvim_win_close(border_win, true)
    vim.api.nvim_win_close(win, true)
  end
end

-- Create a simple floating terminal.
local function floating_terminal(cmd, callback, win_width, win_height, loc)
  local current_window = vim.api.nvim_get_current_win()

  loc = loc or location.center
  win_height = win_height or 40
  win_width = win_width or 60
  cmd = cmd or vim.api.nvim_get_option('shell')

  local width = vim.api.nvim_get_option('columns')
  local height = vim.api.nvim_get_option('lines')
  local buf, win, closer = floating_buffer(math.ceil(width*(win_width/100)), math.ceil(height*(win_height/100)), loc)

  vim.cmd [[ autocmd TermOpen * startinsert ]]
  vim.fn.termopen(cmd, {
    on_exit = function(_, _, _)
      local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
      vim.api.nvim_set_current_win(current_window)
      closer()
      if callback then
        callback(lines)
      end
    end
  })
  return buf, win, closer
end

local function fzf(input, handler)
  if type(input) == 'table' then
    input = string.format("printf '%s'", table.concat(input, '\n'))
  end
  local cmd = string.format('%s | fzf', input)
  floating_terminal(cmd, function(lines) handler(lines[1]) end)
end
local function open_file(file, at)
  at = at or 0
  vim.cmd (string.format('e +%s %s', at, file))
end

local function find(opts)
  opts = opts or {}
  opts.cwd = opts.cwd or '.'
  opts.hidden = opts.hidden or false
  opts.args = opts.args or {}
  local hidden = [[-not -path '*/\.*']]
  if opts.hidden then
    hidden = ''
  end
  local cmd = string.format('find %s %s -type s,f', opts.cwd, hidden)
  fzf(cmd, open_file)
end

local function git_files(opts)
  if not vim.fn.isdirectory('.git') then
    print('Error: not a git repo.')
    return
  end
  fzf('git ls-files', open_file)
end

local function fd(opts)
  opts = opts or {}
  opts.hidden = opts.hidden or false
  if opts.hidden then
    opts.hidden = '--hidden'
  else
    opts.hidden = ''
  end
  local cmd = string.format('fdfind %s --type f --type s', opts.hidden)
  fzf(cmd, open_file)
end

local function git_grep(opts)
  local cmd = 'git grep -n ""'
  fzf(cmd, function(line)
    line = vim.split(line, ':')
    open_file(line[1], line[2])
  end)
end

local function rg(opts)
  local cmd = 'rg --column --line-number --no-heading --ignore-case '
  fzf(cmd, function(line)
    line = vim.split(line, ':')
    open_file(line[1], line[2])
  end)
end

local function buffers()
  local _buffers = {}
  local function buffer_state(buf)
    if vim.api.nvim_buf_is_loaded(buf) then
      return 'L'
    else
      return 'U'
    end
  end
  for _,b in ipairs(vim.api.nvim_list_bufs()) do
    if 1 == vim.fn.buflisted(b) then
      table.insert(_buffers, string.format("[%s] %s:%s", buffer_state(b), b, vim.api.nvim_buf_get_name(b)))
    end
  end
  fzf(_buffers, function(line)
    local buffer_name = vim.split(line, ':')[2]
    vim.cmd(string.format('buffer %s', buffer_name))
  end)
end

local function buffer_lines()
  

end

return {
  terminal = floating_terminal,
  fzf = fzf,
  rg = rg,
  git_grep = git_grep,
  git_files = git_files,
  fd = fd,
  find = find,
  buffers = buffers,
}




