local M = {}
M.vim = {}

local uv = vim.loop

M.vim.filename = '%f'
M.vim.modified = '%m'
M.vim.buffer_lines = '%L'
M.vim.current_line = '%l'
M.vim.column = '%c'
M.vim.percent_of_file = '%p'
M.vim.filetype = '%Y'

-- blocks are parts of statusline in a [ ] pair
function M.block(inner)
  return string.format('[ %s ]', table.concat(inner, ' '))
end

function table.slice(tbl, first, last, step)
  local sliced = {}

  for i = first or 1, last or #tbl, step or 1 do
    sliced[#sliced+1] = tbl[i]
  end

  return sliced
end


function M.create_statusline(opts)
  return table.concat(opts, ' ')
end

function M.space(n)
  return string.rep(' ', n)
end

-- TODO: make a way for user to register a custom function
function M.git_branch()
  os.execute('(git branch -l | grep "*") 2>/dev/null 1> /tmp/statusline_grep') 
  local output = io.open('/tmp/statusline_grep')
  if not output then return 'Not a Git repo' end
  output = output:read('*all')
  return vim.split(vim.split(output, '\n')[1], ' ')[2]
end
local line = M.create_statusline {
  M.block {
    M.vim.filename,
  },
  M.block {
    M.vim.column,
    ':',
    M.vim.current_line,
    ':',
    M.vim.buffer_lines,
  },
  M.block {
    [[%{luaeval ("require('statusline').git_branch()")}]],
  },
}
vim.api.nvim_set_option('statusline', line)
return M

