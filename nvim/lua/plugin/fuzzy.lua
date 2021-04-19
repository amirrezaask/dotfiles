local normal_maps = {}

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
normal_maps['<Space><Space>'] = '<cmd>lua require("fuzzy").find_files{}<CR>'
normal_maps['<Space>fb'] = '<cmd>lua require("fuzzy").interactive_finder{}<CR>'
normal_maps['<Space>ec'] =
  '<cmd>lua require("fuzzy").find_files{path="~/src/github.com/amirrezaask/dotfiles", prompt="Edit dotfiles> "}<CR>'
normal_maps['<Space>en'] = '<cmd>lua require("fuzzy").find_files{path="~/.config/nvim", prompt="Edit Neovim> "}<CR>'
normal_maps['<Space>ez'] =
  '<cmd>lua require("fuzzy").find_files{path="~/src/github.com/amirrezaask/dotfiles/zsh", prompt="Edit ZSH> "}<CR>'
normal_maps['<Space>fp'] =
  '<cmd>lua require("fuzzy").find_repo{locations={"~/.local/share/nvim/site/pack/packer"}, prompt="Find plugins> "}<CR>'
normal_maps['<Space>gf'] = '<cmd>lua require("fuzzy").git_files{}<CR>'
normal_maps['<C-p>'] = '<cmd>lua require("fuzzy").git_files{}<CR>'
normal_maps['<Space>fr'] = '<cmd>lua require"fuzzy".recent_files{}<CR>'
normal_maps['<Space>pf'] = '<cmd>lua require("fuzzy").find_repo{locations={"~/src"}}<CR>'
normal_maps['??'] = '<cmd>lua require("fuzzy").grep{ height = 90, width = 70 }<CR>'
normal_maps['<Space>b'] = '<cmd>lua require("fuzzy").buffers{}<CR>'
normal_maps['<Space>gg'] = '<cmd>lua require("fuzzy").git_grep{}<CR>'
normal_maps['<Space>c'] = '<cmd>lua require("fuzzy").commands{}<CR>'
normal_maps['<Space>h'] = '<cmd>lua require("fuzzy").history{}<CR>'
normal_maps['<Space>h'] = '<cmd>lua require("fuzzy").help{}<CR>'
normal_maps['<Space>gc'] = '<cmd>lua require("fuzzy").git_commits{}<CR>'
normal_maps['<Space>gb'] = '<cmd>lua require("fuzzy").git_bcommits{}<CR>'
normal_maps['<Space>gco'] = '<cmd>lua require("fuzzy").git_checkout{}<CR>'

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
  prompt = '❯ ',
  register = {
    base16_theme_selector = base16_theme_selector,
  },
})

nvim.mode_map({
  n = normal_maps,
})

return fuzzy
