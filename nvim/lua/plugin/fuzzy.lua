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
    handler = function(theme)
      for k, v in pairs(base16.themes) do
        if k == theme then
          base16(v)
        end
      end
    end,
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
  height = 70,
  blacklist = {
    'vendor',
    '.git',
    'target',
  },
  location = loc.center,
  sorter = require('fuzzy.lib.sorter').fzf_native,
  prompt = '❯ ',
  register = {
    base16_theme_selector = base16_theme_selector,
  },
  border = nil,
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
  },
})
nvim.mode_map({
  n = {
    ['<Space><Space>'] = '<cmd>lua require("fuzzy").find_files{}<CR>',
    ['<Space>fb'] = '<cmd>lua require("fuzzy").interactive_finder{}<CR>',
    ['<Space>ec'] = '<cmd>lua require("fuzzy").find_files{path="~/src/github.com/amirrezaask/dotfiles", prompt="Edit dotfiles> "}<CR>',
    ['<Space>en'] = '<cmd>lua require("fuzzy").find_files{path="~/.config/nvim", prompt="Edit Neovim> "}<CR>',
    ['<Space>ez'] = '<cmd>lua require("fuzzy").find_files{path="~/src/github.com/amirrezaask/dotfiles/zsh", prompt="Edit ZSH> "}<CR>',
    ['<Space>fp'] = '<cmd>lua require("fuzzy").find_repo{locations={"~/.local/share/nvim/site/pack/packer"}, prompt="Find plugins> "}<CR>',
    ['<Space>gf'] = '<cmd>lua require("fuzzy").git_files{}<CR>',
    ['<C-p>'] = '<cmd>lua require("fuzzy").git_files{}<CR>',
    ['<Space>fr'] = '<cmd>lua require"fuzzy".recent_files{}<CR>',
    ['<Space>pf'] = '<cmd>lua require("fuzzy").find_repo{locations={"~/src"}}<CR>',
    ['??'] = '<cmd>lua require("fuzzy").grep{ height = 90, width = 70 }<CR>',
    ['<Space>b'] = '<cmd>lua require("fuzzy").buffers{}<CR>',
    ['<Space>gg'] = '<cmd>lua require("fuzzy").git_grep{}<CR>',
    ['<Space>c'] = '<cmd>lua require("fuzzy").commands{}<CR>',
    ['<Space>h'] = '<cmd>lua require("fuzzy").help{}<CR>',
    ['<Space>gc'] = '<cmd>lua require("fuzzy").git_commits{}<CR>',
    ['<Space>gb'] = '<cmd>lua require("fuzzy").git_bcommits{}<CR>',
    ['<Space>gco'] = '<cmd>lua require("fuzzy").git_checkout{}<CR>',
  },
})

return fuzzy
