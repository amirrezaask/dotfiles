-- Side tree file browser using Luv

local uv = vim.loop

local size = vim.g.side_tree_size or 15

local M = {}

local function abbr_type(t)
  if t == 'file' then
    return 'f'
  elseif t == 'directory' then
    return 'd'
  end
end


local function scandir(path, hidden)
  path = path or '.'
  hidden = hidden or false
  local files = {}
  local dir_t = uv.fs_scandir(path)
  assert(dir_t)
  while true do
    local filename, type = uv.fs_scandir_next(dir_t)
    if not filename then
      break
    end
    local stat = uv.fs_stat(path .. '/' .. filename)
    table.insert(files, stat.size .. ' ' .. abbr_type(type) .. ' ' .. filename)
  end
  return files
end

SIDE_FILE_CURRENT_WINDOW = nil
SIDE_FILE_WIN = nil
SIDE_FILE_BUF = nil
SIDE_FILE_CURRENT_PATH = nil

function __SIDE_FILE_BROWSER_OPEN()
  local line = vim.api.nvim_get_current_line()
  local segments = vim.split(line, ' ')
  local filename = table.concat(table.slice(segments, 3, #segments), ' ')
  local type = segments[2]
  if type == 'd' then
    M.open_side_file_browser(SIDE_FILE_CURRENT_PATH .. '/' .. filename)
  elseif type == 'f' then
    local actual_filename = SIDE_FILE_CURRENT_PATH .. '/' .. filename
    vim.api.nvim_set_current_win(SIDE_FILE_CURRENT_WINDOW)
    vim.cmd(string.format([[ e %s ]], actual_filename))
  end
end

function __SIDE_FILE_BROWSER_BACK()
  local segments = vim.split(SIDE_FILE_CURRENT_PATH, '/')
  local last_dir = table.concat(table.slice(segments, 1, #segments - 1), '/')
  M.open_side_file_browser(last_dir) 
end

local function split_size()
  return math.ceil((size * vim.api.nvim_get_option('columns') ) / 100) 
end

function M.open_side_file_browser(path)
  if not path or path == '' then
    path = '.'
  end
  if not SIDE_FILE_CURRENT_WINDOW then
    SIDE_FILE_CURRENT_WINDOW = vim.api.nvim_get_current_win()
  end
  SIDE_FILE_CURRENT_PATH = path
  -- create the buffer
  if not SIDE_FILE_BUF then
    vim.cmd [[ set nospr ]]
    vim.cmd [[ vnew ]]
    vim.cmd(string.format([[ vertical resize %s]], split_size()))
    SIDE_FILE_BUF = vim.api.nvim_get_current_buf()
  end
  vim.api.nvim_buf_set_option(SIDE_FILE_BUF, 'buftype', 'nowrite')

  if not SIDE_FILE_WIN then
    SIDE_FILE_WIN = vim.api.nvim_get_current_win()
  end
  vim.api.nvim_buf_set_keymap(SIDE_FILE_BUF, 'n', '<CR>', '<cmd>lua __SIDE_FILE_BROWSER_OPEN()<CR>', {})
  vim.api.nvim_buf_set_keymap(SIDE_FILE_BUF, 'n', '<BS>', '<cmd>lua __SIDE_FILE_BROWSER_BACK()<CR>', {})
  vim.api.nvim_buf_set_keymap(SIDE_FILE_BUF, 'n', 'd', '<cmd>lua __SIDE_FILE_BROWSER_DELETE()<CR>', {})

  local files = scandir(path, true)
  vim.api.nvim_buf_set_lines(SIDE_FILE_BUF, 0, -1, false, files)
end

return M
