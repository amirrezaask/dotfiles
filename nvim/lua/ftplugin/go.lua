local go = {}
local floating = require('amirrezaask.floating')

function go.imports(filename)
  filename = filename or vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf())
  vim.cmd(string.format([[ silent ! goimports -w %s ]], filename))
  vim.cmd([[ silent ! e ]])
end

function go.fmt(pkg)
  pkg = pkg or '.'
  vim.cmd(string.format('silent ! go fmt %s', pkg))
end


-- this should open a floating window with output of go test 
function go.test()
  floating:new {
    source = 'go test -v ./...'
  }
end
vim.cmd [[ command! GoTest lua require'ftplugin.go'.test() ]]

function go.build()
  floating:new {
    source = 'go build ./...'
  }
end

vim.cmd([[ command! GoBuild lua require'ftplugin.go'.build() ]])

local function default_formatter()
  if vim.fn.executable('goimports') then
    return 'goimports'
  else
    return 'gofmt'
  end
end
if default_formatter() == 'goimports' then
  vim.cmd([[autocmd BufWritePost <buffer> lua require('ftplugin.go').imports()]])
else
  vim.cmd([[autocmd BufWritePost <buffer> lua require('ftplugin.go').fmt()]])
end

return go
