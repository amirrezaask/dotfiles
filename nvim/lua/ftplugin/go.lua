local go = {}
local floating = require('amirrezaask.floating')
local nvim = require('amirrezaask.nvim')

-- deps:
-- gomodifytags github.com/fatih/gomodifytags
-- goimports golang.org/x/tools/cmd/goimports
-- github.com/davidrjenni/reftools/cmd/fillstruct

function go.imports(filename)
  filename = filename or vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf())
  vim.cmd(string.format([[ silent ! goimports -w %s ]], filename))
  vim.cmd [[ silent e ]]
end

function go.fmt(pkg)
  pkg = pkg or '.'
  vim.cmd(string.format('silent go fmt %s', pkg))
  vim.cmd [[ silent e ]]
end

function go.add_tags()

end

function go.test()
  floating:new {
    source = 'go test -v ./...'
  }
end
function go.build()
  floating:new {
    source = 'go build ./...'
  }
end

local function default_formatter()
  if vim.fn.executable('goimports') then
    return 'lua require("ftplugin.go").imports()'
  else
    return 'lua require("ftplugin.go").fmt()'
  end
end

local formatter = default_formatter()

vim.cmd(string.format([[autocmd BufWritePost <buffer> %s]], formatter))

nvim.command('GoTest', go.test)
nvim.command("GoBuild", go.build)
return go
