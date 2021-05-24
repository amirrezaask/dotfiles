local actions = require('actions')

actions:setup {
  mappings = {
    ['n <Space>ab'] = 'build'
  },
  filetypes = {
    lua = {
      build = function()
        print('lua build command')
      end
    },
    go = {
      build = function()
        print('go build command')
      end
    }
  }
}

