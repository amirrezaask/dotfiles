local M = {}

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
    if type(v) == 'boolean' then
      if v == true then
        vim.cmd(string.format([[set %s]], n))
      else
        vim.cmd(string.format([[set no%s]], n))
      end
    else
      vim.api.nvim_set_option(n, v)
    end
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

__FUNCTION_REGISTRY = {}

function M.command(name, expr, args)
  if type(expr) == 'function' then
    local fn = expr
    __FUNCTION_REGISTRY[name] = function()
      fn()
    end
    expr = string.format('lua __FUNCTION_REGISTRY["%s"]()<CR>', name)
  end
  if not args then
    vim.cmd(string.format('command! %s %s', name, expr))
  end
  if args then
    vim.cmd(string.format('command! -nargs=%s %s %s', args, name, expr))
  end
end
return M
