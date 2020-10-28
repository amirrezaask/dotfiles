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

function M.segments(segs)
  return table.concat(segs, '')
end

function M.git_branch()
  local stdout = io.popen("git branch -l | grep '*'")
  local output = stdout:read('*all') 
  stdout:close()
  if output:find('fatal') then
    return 'Not a Git repo'
  end
  output = vim.split(output, ' ')[2]
  output = vim.split(output, '\n')[1]
  return M.block {
    'Git: ' .. output,
  }
end

local line = M.segments {
  M.block {
    M.vim.column,
    ':',
    M.vim.current_line,
    ':',
    M.vim.buffer_lines,
  },
  M.block {
    M.vim.filename
  },
  [[%{luaeval ("require('statusline').git_branch()")}]],
  M.block {
    'Filetype: ' .. M.vim.filetype
  }
}

vim.api.nvim_set_option('statusline', line)
return M

