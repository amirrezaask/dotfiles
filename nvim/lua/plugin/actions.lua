local actions = require('actions')
local floating = require('floating')

local function floating_window_opts(opts)
  local base = {
    height_pct = 50,
    width_pct = 70
  }
  if opts then
    for k,v in pairs(opts) do
      base[k] = v
    end
  end
  return base
end

actions:setup {
  mappings = {
    ['n ,ab'] = 'build'
    ['n ,at'] = 'test_all'
  },
  filetypes = {
    go = {
      build = function(bufnr)
        floating:command('go build', floating_window_opts {
          cwd = vim.fn.expand('%:p:h')
        })
      end,
      test_all = function(bufnr)
        floating:command('go test -v ./...', floating_window_opts {
          jobstart = {
            cwd = vim.fn.expand('%:p:h')
          }
        }))
      end,
    }
  }
}

