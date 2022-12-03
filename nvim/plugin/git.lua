use {
  "lewis6991/gitsigns.nvim",
}

use {
  "junegunn/gv.vim",
}

use {
  "cohama/agit.vim",
}

use {
  "TimUntersberger/neogit",
  requires = "nvim-lua/plenary.nvim",
}

use {
  "tpope/vim-fugitive",
}

function configs.gitsigns()
  require("gitsigns").setup {}
  require("neogit").setup {
    kind = "vsplit",
  }
  require("core.keymaps").nnoremap("<leader>g", "<cmd>Git<cr>")
end
