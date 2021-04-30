local treesitter = require('nvim-treesitter.configs')
treesitter.setup({
  ensure_installed = { 'go', 'lua', 'python', 'rust' },
  highlight = {
    enable = true,
  },
  keymaps = {
    init_selection = 'gnn',
    node_incremental = 'grn',
    scope_incremental = 'grc',
    node_decremental = 'grm',
  },
  indent = {
    enable = true,
  },
  textobjects = {
    -- @block.inner
    -- @block.outer
    -- @call.inner
    -- @call.outer
    -- @class.inner
    -- @class.outer
    -- @comment.outer
    -- @conditional.inner
    -- @conditional.outer
    -- @frame.inner
    -- @frame.outer
    -- @function.inner
    -- @function.outer
    -- @loop.inner
    -- @loop.outer
    -- @parameter.inner
    -- @parameter.outer
    -- @scopename.inner
    -- @statement.outer
    select = {
      enable = true,
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        ['ac'] = '@class.outer',
        ['ic'] = '@class.inner',
      },
    },
  },
})
