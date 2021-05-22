local treesitter = require('nvim-treesitter.configs')
treesitter.setup({
  ensure_installed = { 'go', 'lua', 'python', 'rust', 'query', 'toml', 'php', 'yaml', 'json', 'dockerfile', 'c', 'gomod', 'html' },
  context_commentstring = {
    enable = true,
    config = {
      c   = '// %s',
      lua = '-- %s',
    },
  },
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
