-- Integrate Tmux and neovim pane and window switching
plugin "mrjones2014/smart-splits.nvim"
plugin "christoomey/vim-tmux-navigator"
plugin "pbrisbin/vim-mkdir"
plugin "sheerun/vim-polyglot"
plugin "szw/vim-maximizer"
plugin "fladson/vim-kitty"
plugin "towolf/vim-helm"
plugin "jansedivy/jai.vim"
plugin {
  "folke/todo-comments.nvim",
  requires = "nvim-lua/plenary.nvim",
}
plugin "tpope/vim-surround"
plugin "junegunn/vim-easy-align"
plugin {
  "numToStr/Comment.nvim",
}
plugin "windwp/nvim-autopairs"
plugin { "L3MON4D3/LuaSnip", tag = "v<CurrentMajor>.*", requires = "rafamadriz/friendly-snippets" }
plugin "junegunn/goyo.vim"

require "jasvim.editor.treesitter"
require("todo-comments").setup()
require("Comment").setup {
  ---Add a space b/w comment and the line
  padding = true,
  ---Whether the cursor should stay at its position
  sticky = true,
  ---Lines to be ignored while (un)comment
  ---LHS of toggle mappings in NORMAL mode
  toggler = {
    ---Line-comment toggle keymap
    line = "gcc",
    ---Block-comment toggle keymap
    block = "gbc",
  },
  ---LHS of operator-pending mappings in NORMAL and VISUAL mode
  opleader = {
    ---Line-comment keymap
    line = "gc",
    ---Block-comment keymap
    block = "gb",
  },
  ---LHS of extra mappings
  extra = {
    ---Add comment on the line above
    above = "gcO",
    ---Add comment on the line below
    below = "gco",
    ---Add comment at the end of line
    eol = "gcA",
  },
  ---Enable keybindings
  mappings = {
    ---Operator-pending mapping; `gcc` `gbc` `gc[count]{motion}` `gb[count]{motion}`
    basic = true,
    ---Extra mapping; `gco`, `gcO`, `gcA`
    extra = true,
  },
}
-- ]]

require("luasnip.loaders.from_vscode").lazy_load()

require("nvim-autopairs").setup {}
