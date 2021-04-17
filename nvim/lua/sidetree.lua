-- Side tree file browser using Luv

local uv = vim.loop

local size = vim.g.side_tree_size or 15

local M = {}
function table.slice(tbl, first, last, step)
  local sliced = {}
  for i = first or 1, last or #tbl, step or 1 do
    sliced[#sliced+1] = tbl[i]
  end
  return sliced
end
local function abbr_type(t)
  if t == 'file' then
    return 'f'
  elseif t == 'directory' then
    return 'd'
  elseif t == 'link' then
    return 'l'
  end
end

local function scandir(path, hidden)
  path = path or '.'
  hidden = hidden or false
  local _files = {}
  local dir_t = uv.fs_scandir(path)
  assert(dir_t)
  while true do
    local filename, type = uv.fs_scandir_next(dir_t)
    if not filename then
      break
    end
    local stat = uv.fs_stat(path .. '/' .. filename)
    table.insert(_files, { stat.size,abbr_type(type),filename})
  end
  local biggest_size = 0
  for i, file in pairs(_files) do
    if file[1] > biggest_size then
      biggest_size = file[1]
    end
  end
  local files_out = {}
  for i, file in pairs(_files) do
    file[1] = string.format('%d', file[1]) .. string.rep(' ', #string.format("%d", biggest_size) - #string.format("%d", file[1])) 
    if file[1] and file[2] and file[3] then
      table.insert(files_out, file[1] .. ' ' .. file[2] .. ' ' .. file[3])
    end
  end
  return files_out
end

M.SIDE_FILE_CURRENT_WINDOW = nil
M.SIDE_FILE_WIN = nil
M.SIDE_FILE_BUF = nil
M.SIDE_FILE_CURRENT_PATH = nil

function __SIDE_FILE_BROWSER_OPEN()
  local line = vim.api.nvim_get_current_line()
  local segments = vim.split(line, ' ')
  local filename = table.concat(table.slice(segments, 3, #segments), ' ')
  local type = segments[2]
  if type == 'd' then
    M.open_side_file_browser(M.SIDE_FILE_CURRENT_PATH .. '/' .. filename)
  elseif type == 'f' then
    local actual_filename = M.SIDE_FILE_CURRENT_PATH .. '/' .. filename
    vim.api.nvim_set_current_win(M.SIDE_FILE_CURRENT_WINDOW)
    vim.cmd(string.format([[ e %s ]], actual_filename))
  end
end

function __SIDE_FILE_BROWSER_CLOSE()
  vim.api.nvim_win_close(M.SIDE_FILE_WIN, true)
  M.SIDE_FILE_BUF = nil
  M.SIDE_FILE_WIN = nil
  M.SIDE_FILE_CURRENT_PATH = nil
  M.SIDE_FILE_CURRENT_WINDOW = nil
end

function __SIDE_FILE_BROWSER_BACK()
  local segments = vim.split(M.SIDE_FILE_CURRENT_PATH, '/')
  local last_dir = table.concat(table.slice(segments, 1, #segments - 1), '/')
  M.open_side_file_browser(last_dir) 
end

function __SIDE_FILE_BROWSER_DELETE()
  local line = vim.api.nvim_get_current_line()
  local segments = vim.split(line, ' ')
  local filename = table.concat(table.slice(segments, 3, #segments), ' ')
  local actual_filename = M.SIDE_FILE_CURRENT_PATH .. '/' .. filename
  vim.cmd(string.format([[ ! rm -rf %s ]], actual_filename))
  M.open_side_file_browser(M.SIDE_FILE_CURRENT_PATH)
end

function __SIDE_FILE_BROWSER_VS()
  local line = vim.api.nvim_get_current_line()
  local segments = vim.split(line, ' ')
  local filename = table.concat(table.slice(segments, 3, #segments), ' ')
  vim.api.nvim_set_current_win(M.SIDE_FILE_CURRENT_WINDOW)
  local actual_filename = M.SIDE_FILE_CURRENT_PATH .. '/' .. filename
  vim.cmd(string.format([[ vs %s ]], actual_filename))
  vim.api.nvim_set_current_win(M.SIDE_FILE_WIN)
end

function __SIDE_FILE_BROWSER_SP()
  local line = vim.api.nvim_get_current_line()
  local segments = vim.split(line, ' ')
  local filename = table.concat(table.slice(segments, 3, #segments), ' ')
  vim.api.nvim_set_current_win(M.SIDE_FILE_CURRENT_WINDOW)
  local actual_filename = M.SIDE_FILE_CURRENT_PATH .. '/' .. filename
  vim.cmd(string.format([[ sp %s ]], actual_filename))
  vim.api.nvim_set_current_win(M.SIDE_FILE_WIN)
end

local function split_size()
  return math.ceil((size * vim.api.nvim_get_option('columns') ) / 100) 
end

function M.open_side_file_browser(path)
  if not path or path == '' then
    path = '.'
  end
  if not M.SIDE_FILE_CURRENT_WINDOW then
    M.SIDE_FILE_CURRENT_WINDOW = vim.api.nvim_get_current_win()
  end
  M.SIDE_FILE_CURRENT_PATH = path

  if M.SIDE_FILE_BUF == nil or M.SIDE_FILE_WIN == nil or 
    (not vim.api.nvim_buf_is_loaded(M.SIDE_FILE_BUF) or not vim.api.nvim_win_is_valid(M.SIDE_FILE_WIN)) then
    vim.cmd [[ set nospr ]]
    vim.cmd [[ vnew ]]
    vim.cmd(string.format([[ vertical resize %s]], split_size()))
    M.SIDE_FILE_BUF = vim.api.nvim_get_current_buf()
    M.SIDE_FILE_WIN = vim.api.nvim_get_current_win()
  end
  vim.api.nvim_buf_set_option(M.SIDE_FILE_BUF, 'buftype', 'nofile')
  vim.api.nvim_buf_set_option(M.SIDE_FILE_BUF, 'modifiable', true)

  if not M.SIDE_FILE_WIN then
    M.SIDE_FILE_WIN = vim.api.nvim_get_current_win()
  end

  vim.api.nvim_set_current_win(M.SIDE_FILE_WIN)
  vim.api.nvim_buf_set_keymap(M.SIDE_FILE_BUF, 'n', '<CR>', '<cmd>lua __SIDE_FILE_BROWSER_OPEN()<CR>', {})
  vim.api.nvim_buf_set_keymap(M.SIDE_FILE_BUF, 'n', '<BS>', '<cmd>lua __SIDE_FILE_BROWSER_BACK()<CR>', {})
  vim.api.nvim_buf_set_keymap(M.SIDE_FILE_BUF, 'n', 'q', '<cmd>lua __SIDE_FILE_BROWSER_CLOSE()<CR>', {})
  vim.api.nvim_buf_set_keymap(M.SIDE_FILE_BUF, 'n', 's', '<cmd>lua __SIDE_FILE_BROWSER_SP()<CR>', {})
  vim.api.nvim_buf_set_keymap(M.SIDE_FILE_BUF, 'n', 'v', '<cmd>lua __SIDE_FILE_BROWSER_VS()<CR>', {})
  vim.api.nvim_buf_set_keymap(M.SIDE_FILE_BUF, 'n', 'd', '<cmd>lua __SIDE_FILE_BROWSER_DELETE()<CR>', {})

  local files = scandir(path, true)
  vim.api.nvim_buf_set_lines(M.SIDE_FILE_BUF, 0, -1, false, files)
  vim.api.nvim_buf_set_option(M.SIDE_FILE_BUF, 'modifiable', false)
end
return M
