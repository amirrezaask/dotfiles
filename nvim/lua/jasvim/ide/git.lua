plugin "tpope/vim-fugitive"
plugin "junegunn/gv.vim"
plugin "cohama/agit.vim"
plugin {
  "lewis6991/gitsigns.nvim",
}
plugin {
  "TimUntersberger/neogit",
  requires = "nvim-lua/plenary.nvim",
}

require("neogit").setup()
require("gitsigns").setup()
