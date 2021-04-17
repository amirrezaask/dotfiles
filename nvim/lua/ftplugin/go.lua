local go = {}

function go.imports(filename)
  filename = filename or vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf())
  vim.cmd(string.format([[ silent goimports -w %s ]], filename))
end
function go.fmt(pkg)
  pkg = pkg or '.'
  vim.cmd(string.format('silent ! go fmt %s', pkg))
end
local function default_formatter()
  if vim.fn.executable('goimports') then
    return 'goimports'
  else
    return 'gofmt'
  end
end
if default_formatter() == 'goimports' then
  vim.cmd [[autocmd BufWritePre *.go lua require('ftplugin.go').imports()]]
else
  vim.cmd [[autocmd BufWritePre *.go lua require('ftplugin.go').fmt()]]
end

return go
