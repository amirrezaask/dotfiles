local M = {}

local quickfix_state = 'close'

function M.toggle()
  local current_win = vim.api.nvim_get_current_win()
  if quickfix_state == 'open' then 
    quickfix_state = 'close'
    vim.c.cclose()
  else
    quickfix_state = 'open'
    vim.c.copen()
  end
  vim.api.nvim_set_current_win(current_win)
end

vim.map {
  ['n <C-q>'] = M.toggle,
  ['n {'] = vim.c.cprev,
  ['n }'] = vim.c.cnext,
}

local function gen_from_cmd_output(qflist, data)
  for _, l in ipairs(data) do
    if l ~= '' then
      local splitted = vim.split(l, ':')
      if #splitted > 2 then 
        local entry = {filename=splitted[1], lnum=tonumber(splitted[2]), col=tonumber(splitted[3]), text=splitted[4]}
        if entry.filename ~= '' and entry.lnum ~= '' then
          table.insert(qflist, entry)
        end
      end
    end
  end
  return qflist 
end

function M.quickfix_from_cmd(cmd)
  vim.fn.jobstart(cmd, {
    on_exit = function(_, exit_code, _)
      if exit_code ~= 0 then print('exit code ' .. exit_code) end
    end,
    on_stdout = function(_, data, _)
      local qflist = vim.fn.getqflist()
      qflist = gen_from_cmd_output(qflist, data)
      vim.fn.setqflist(qflist)
    end,
    on_stderr = function(_, data, _)
      local qflist = vim.fn.getqflist()
      qflist = gen_from_cmd_output(qflist, data)
      vim.fn.setqflist(qflist)
    end,
  })
end

function M.quickfix_entry(filename, lnum, col, text)
  return {filename=filename, lnum=lnum, col=col, text=text}
end

return M
