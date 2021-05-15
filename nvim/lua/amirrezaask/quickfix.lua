local M = {}
local nvim = require('amirrezaask.nvim')

local quickfix_state = 'close'

function M.toggle()
  if quickfix_state == 'open' then 
    quickfix_state = 'close'
    vim.cmd [[ cclose ]]
  else
    quickfix_state = 'open'
    vim.cmd [[ copen ]]
  end
end

nvim.map {
  ['n <C-q>'] = '<cmd>lua require"amirrezaask.quickfix".toggle()<CR>',
  ['n {'] = ':cprev<CR>',
  ['n }'] = ':cnext<CR>',
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
