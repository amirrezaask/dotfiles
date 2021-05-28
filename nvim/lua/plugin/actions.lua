local actions = require('actions')
local floating = require('floating')
local lsp = require('plugin.lsp')

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
    ['n ,ab'] = 'build',
    ['n ,at'] = 'test_all',
    ['n ,tt'] = 'test_this',
    ['n ,ar'] = 'run',
  },
  filetypes = {
    lua = {
      run = function(bufnr)
        local name = vim.api.nvim_buf_get_name(bufnr)        
        local loader = loadfile(name)
        local env = setmetatable({
          print = vim.schedule_wrap(print)
        },
        {
          __index = _G,
          __newindex = _G
        })
        pcall(setfenv(loader, env))
      end
    },
    go = {
      build = function(_)
        floating:command('go build', floating_window_opts {
          cwd = lsp.go_root()
        })
      end,
      test_all = function(_)
        floating:command('go test -v ./...', floating_window_opts {
          jobstart = {
            cwd = lsp.go_root()
          }
        })
      end,
      test_this = function(_)
        local function go_current_test()
          local linenr = vim.fn.search("func \\(Test\\|Example\\)", "bcnW")
          if linenr == 0 then return end
          local linetext = vim.fn.getline(linenr)
          local test_name = vim.split(linetext, ' ')[2]
          local start, _ = string.find(test_name, '%(')
          test_name = string.sub(test_name, 1, start-1)
          return test_name
        end
        local current_test = go_current_test()
        floating:command(('go test -v -run %s'):format(current_test), floating_window_opts {
         jobstart = {
            cwd = lsp.go_root()
          }
        })
      end,
    }
  }
}
