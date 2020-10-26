local M = {}

--[[
  {
    group = {
      {"event", "filter", "command"}, 
    }
  }
--]]
--
function M.autocmd(tbl)
  vim.cmd(string.format('autocmd! %s %s %s', tbl[1], tbl[2], tbl[3]))
end

function M.augroup(tbl)
  for g, _ in pairs(tbl) do
    vim.cmd('augroup ' .. g)
    M.autocmd(tbl[g])
    vim.cmd('augroup END')
  end
end

function M.with_options(tbl)
  for n, v in pairs(tbl) do
    vim.api.nvim_set_option(n, v)
  end
end

function M.mode_map(tbl)
  for m, _ in pairs(tbl) do
    for k, expr in pairs(tbl[m]) do
      vim.api.nvim_set_keymap(m, k, expr, {})
    end
  end
end

function M.map(tbl)
  for k, expr in pairs(tbl) do
    vim.api.nvim_set_keymap('', k, expr, {})
  end
end

return M

