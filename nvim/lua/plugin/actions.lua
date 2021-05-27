local actions = require('actions')

actions:setup {
  mappings = {
    ['n <Space>ab'] = 'build'
  },
  filetypes = {
    lua = {
      build = function(bufnr)
        print('lua build command')
      end
    },
    go = {
      build = function(bufnr)
        local root = utils.get_root(bufnr)
        print('go build command')
      end
    }
  }
}

