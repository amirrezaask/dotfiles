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
        local theme = CurrentFuzzy():get_output()
        for k, v in pairs(base16.themes) do
          if k == theme then
            base16(v)
          end
        end
      end,
    },
  })
end

-- vim.cmd([[ highlight FuzzyMatching guifg=#f2904b guibg=none guisp=none ]])

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
  sorter = require('fuzzy.lib.sorter').fzy_native,
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
    ['<leader>A'] = '<cmd>LSPCodeActions<CR>',
    ['<leader>R'] = '<cmd>LSPRename<CR>',
    ['<leader>lr'] = '<cmd>LSPReferences<CR>',
    ['<leader>li'] = '<cmd>LSPImplementations<CR>',
    ['<leader>ld'] = '<cmd>LSPDocumentSymbols<CR>',
    ['<leader>lw'] = '<cmd>LSPWorkspaceSymbols<CR>',
    ['<leader>lc'] = '<cmd>LSPCodeActions<CR>',
  },
})
nvim.mode_map({
  n = {
    ['<leader><leader>'] = require('fuzzy').find_files,
    ['<leader>fb'] = require('fuzzy').interactive_finder,
    ['<leader>ec'] = function()
      require('fuzzy').find_files({ path = '~/src/github.com/amirrezaask/dotfiles', prompt = 'Edit dotfiles> ' })
    end,
    ['<leader>en'] = function()
      require('fuzzy').find_files({ path = '~/.config/nvim', prompt = 'Edit Neovim> ' })
    end,
    ['<leader>ez'] = function()
      require('fuzzy').find_files({ path = '~/src/github.com/amirrezaask/dotfiles/zsh', prompt = 'Edit ZSH> ' })
    end,
    ['<leader>fp'] = function()
      require('fuzzy').find_repo({ locations = { '~/.local/share/nvim/site/pack/packer' }, prompt = 'Find plugins> ' })
    end,
    ['<leader>gf'] = function()
      require('fuzzy').git_files({})
    end,
    ['<C-p>'] = require('fuzzy').git_files,
    ['<leader>fr'] = require('fuzzy').recent_files,
    ['<leader>pf'] = function()
      require('fuzzy').find_repo({ locations = { '~/src' } })
    end,
    ['??'] = function()
      require('fuzzy').grep({ height = 90, width = 70 })
    end,
    ['<leader>b'] = require('fuzzy').buffers,
    ['<leader>gg'] = require('fuzzy').git_grep,
    ['<leader>c'] = require('fuzzy').commands,
    ['<leader>h'] = require('fuzzy').help,
    ['<leader>gc'] = require('fuzzy').git_commits,
    ['<leader>gb'] = require('fuzzy').git_bcommits,
    ['<leader>gco'] = require('fuzzy').git_checkout,
  },
})

return fuzzy
