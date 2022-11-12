local use = require'core.plugin_manager'.use

use 'sheerun/vim-polyglot'
use 'windwp/nvim-spectre'                         -- Search/Replace project wide
use 'pbrisbin/vim-mkdir'                              -- Save files and create not existing directories
use 'tpope/vim-commentary'
use 'tpope/vim-surround'                              -- Now you can command your surroundings
use 'junegunn/vim-easy-align'                         -- Align text with ease
use 'fladson/vim-kitty'
use {
    "folke/todo-comments.nvim",
    requires = "nvim-lua/plenary.nvim",
}


