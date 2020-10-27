local M = {}
M.vim = {}

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
  M.block {
    M.vim.filetype
  }
}

vim.api.nvim_set_option('statusline', line)
return M

