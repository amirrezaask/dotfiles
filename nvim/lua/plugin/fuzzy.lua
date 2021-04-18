local normal_maps = {}

local loc = require('fuzzy.lib.location')

require('fuzzy.lib.options').setup({
  width = 60,
  height = 60,
  blacklist = {
    'vendor',
    '.git',
    'target',
  },
  location = loc.center,
  sorter = require('fuzzy.lib.sorter').fzy_native,
  prompt = '❯ ',
})

vim.cmd([[ highlight FuzzyMatching guifg=#f2904b guibg=none guisp=none ]])

local nvim = require('amirrezaask.nvim')
local base16 = require('base16')

function Base16ThemeSelector()
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
nvim.command('Base16ThemeSelector', Base16ThemeSelector)
vim.cmd([[ nnoremap <Space>ct <cmd>lua Base16ThemeSelector()<CR> ]])

nvim.command('FileBrowser', require('fuzzy').interactive_finder)
nvim.command('Files', require('fuzzy').find_files)
nvim.command('Grep', require('fuzzy').grep)
nvim.command('MRU', require('fuzzy.vim').mru)
nvim.command('BLines', require('fuzzy').buffer_lines)
nvim.command('Cd', require('fuzzy').cd)
nvim.command('Help', require('fuzzy.vim').help)
nvim.command('Maps', require('fuzzy.vim').mappings)
nvim.command('GitFiles', require('fuzzy.git').git_files)
nvim.command('GitGrep', require('fuzzy.git').git_grep)
nvim.command('GitCommits', require('fuzzy.git').git_commits)
nvim.command('GitBCommits', require('fuzzy.git').git_bcommits)
nvim.command('GitCheckout', require('fuzzy.git').git_checkout)
nvim.command('Buffers', require('fuzzy.vim').buffers)
nvim.command('Rg', require('fuzzy').rg)
nvim.command('Colors', require('fuzzy.vim').colors)

local fuzzy = {}

function fuzzy.grep()
  require('fuzzy').grep({ height = 90, width = 70 })
end

-- Fuzzy.nvim
normal_maps['<Space><Space>'] = '<cmd>lua require("fuzzy").find_files{}<CR>'
normal_maps['<Space>fb'] = '<cmd>lua require("fuzzy").interactive_finder{}<CR>'
normal_maps['<Space>ec'] =
  '<cmd>lua require("fuzzy").find_files{path="~/src/github.com/amirrezaask/dotfiles", prompt="Edit dotfiles> "}<CR>'
normal_maps['<Space>en'] = '<cmd>lua require("fuzzy").find_files{path="~/.config/nvim", prompt="Edit Neovim> "}<CR>'
normal_maps['<Space>ez'] =
  '<cmd>lua require("fuzzy").find_files{path="~/src/github.com/amirrezaask/dotfiles/zsh", prompt="Edit ZSH> "}<CR>'
normal_maps['<Space>fp'] =
  '<cmd>lua require("fuzzy.vim").projects{locations={"~/.local/share/nvim/site/pack/packer"}, prompt="Find plugins> "}<CR>'
normal_maps['<Space>gf'] = '<cmd>lua require("fuzzy.git").git_files{}<CR>'
normal_maps['<C-p>'] = '<cmd>lua require("fuzzy.git").git_files{}<CR>'
normal_maps['<Space>fr'] = '<cmd>lua require"fuzzy.vim".mru{}<CR>'
normal_maps['<Space>pf'] = '<cmd>lua require("fuzzy.vim").projects{locations={"~/src"}}<CR>'
normal_maps['??'] = '<cmd>lua require("plugin.fuzzy").grep{}<CR>'
normal_maps['<Space>b'] = '<cmd>lua require("fuzzy.vim").buffers{}<CR>'
normal_maps['<Space>gg'] = '<cmd>lua require("fuzzy.git").git_grep{}<CR>'
normal_maps['<Space>c'] = '<cmd>lua require("fuzzy.vim").commands{}<CR>'
normal_maps['<Space>h'] = '<cmd>lua require("fuzzy.vim").history{}<CR>'
normal_maps['<Space>h'] = '<cmd>lua require("fuzzy.vim").help{}<CR>'
normal_maps['<Space>gc'] = '<cmd>lua require("fuzzy.git").git_commits{}<CR>'
normal_maps['<Space>gb'] = '<cmd>lua require("fuzzy.git").git_bcommits{}<CR>'
normal_maps['<Space>gco'] = '<cmd>lua require("fuzzy.git").git_checkout{}<CR>'

nvim.mode_map({
  n = normal_maps,
})

return fuzzy
