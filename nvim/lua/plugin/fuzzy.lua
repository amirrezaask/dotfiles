local loc = require('fuzzy.lib.location')

local nvim = require('amirrezaask.nvim')

local function base16_theme_selector()
  local base16 = require('base16')
  local theme_names = {}
  for k, _ in pairs(base16.themes) do
    table.insert(theme_names, k)
  end
  require('fuzzy.lib').new({
    source = theme_names,
    mappings = {
      ['<CR>'] = function()
        local theme = FuzzyInstance():get_output()
        for k, v in pairs(base16.themes) do
          if k == theme then
            base16(v)
          end
        end
      end,
    },
  })
end

vim.cmd([[ highlight FuzzyMatching guifg=#f2904b guibg=none guisp=none ]])

local fuzzy = {}

function fuzzy.grep()
  require('fuzzy').grep()
end

-- Fuzzy.nvim
require('fuzzy').setup({
  width = 60,
  height = 60,
  blacklist = {
    'vendor',
    '.git',
    'target',
  },
  location = loc.center,
  sorter = require('fuzzy.lib.sorter').fzf_native,
  prompt = '> ',
  register = {
    base16_theme_selector = base16_theme_selector,
  },
  border = 'yes',
})

local fuzzy_lsp = require('fuzzy.lsp')
nvim.command('LSPDefinitions', fuzzy_lsp.definitions)
nvim.command('LSPHover', vim.lsp.buf.hover)
nvim.command('LSPSignatureHelp', vim.lsp.buf.signature_help)
nvim.command('LSPTypeDefinition', vim.lsp.buf.type_definition)
nvim.command('LSPRename', require('lspsaga.rename').rename)
nvim.command('LSPWorkspaceSymbols', fuzzy_lsp.workspace_symbols)
nvim.command('LSPDocumentSymbols', fuzzy_lsp.document_symbols)
nvim.command('LSPReferences', fuzzy_lsp.references)
nvim.command('LSPImplementations', fuzzy_lsp.implementation)
nvim.command('LSPCodeActions', fuzzy_lsp.code_actions)
nvim.command('LSPDeclaration', fuzzy_lsp.declaration)
nvim.mode_map({
  n = {
    ['gd'] = '<cmd>LSPDefinitions<CR>',
    ['K'] = '<cmd>LSPHover<CR>',
    ['gI'] = '<cmd>LSPImplementations<CR>',
    ['<c-k>'] = '<cmd>LSPSignatureHelp<CR>',
    ['1gD'] = '<cmd>LSPTypeDefinition<CR>',
    ['gR'] = '<cmd>LSPReferences<CR>',
    ['g0'] = '<cmd>LSPDocumentSymbols<CR>',
    ['gW'] = '<cmd>LSPWorkspaceSymbols<CR>',
    ['gD'] = '<cmd>LSPDeclaration<CR>',
    ['<Space>A'] = '<cmd>LSPCodeActions<CR>',
    ['<Space>R'] = '<cmd>LSPRename<CR>',
    ['<Space>lr'] = '<cmd>LSPReferences<CR>',
    ['<Space>li'] = '<cmd>LSPImplementations<CR>',
    ['<Space>ld'] = '<cmd>LSPDocumentSymbols<CR>',
    ['<Space>lw'] = '<cmd>LSPWorkspaceSymbols<CR>',
    ['<Space>lc'] = '<cmd>LSPCodeActions<CR>',
  },
})
nvim.mode_map({
  n = {
    ['<Space><Space>'] = require('fuzzy').find_files,
    ['<Space>fb'] = require('fuzzy').interactive_finder,
    ['<Space>ec'] = function()
      require('fuzzy').find_files({ path = '~/src/github.com/amirrezaask/dotfiles', prompt = 'Edit dotfiles> ' })
    end,
    ['<Space>en'] = function()
      require('fuzzy').find_files({ path = '~/.config/nvim', prompt = 'Edit Neovim> ' })
    end,
    ['<Space>ez'] = function()
      require('fuzzy').find_files({ path = '~/src/github.com/amirrezaask/dotfiles/zsh', prompt = 'Edit ZSH> ' })
    end,
    ['<Space>fp'] = function()
      require('fuzzy').find_repo({ locations = { '~/.local/share/nvim/site/pack/packer' }, prompt = 'Find plugins> ' })
    end,
    ['<Space>gf'] = function()
      require('fuzzy').git_files({})
    end,
    ['<C-p>'] = require('fuzzy').git_files,
    ['<Space>fr'] = require('fuzzy').recent_files,
    ['<Space>pf'] = function()
      require('fuzzy').find_repo({ locations = { '~/src' } })
    end,
    ['??'] = function()
      require('fuzzy').grep({ height = 90, width = 70 })
    end,
    ['<Space>b'] = require('fuzzy').buffers,
    ['<Space>gg'] = require('fuzzy').git_grep,
    ['<Space>c'] = require('fuzzy').commands,
    ['<Space>h'] = require('fuzzy').help,
    ['<Space>gc'] = require('fuzzy').git_commits,
    ['<Space>gb'] = require('fuzzy').git_bcommits,
    ['<Space>gco'] = require('fuzzy').git_checkout,
  },
})

return fuzzy
