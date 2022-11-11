local plugin_manager = require"core.plugin_manager"
local use = plugin_manager.use
local modules = require"core.modules"

modules {
    "basics",
    "ui",
    "navigation",
    "completion",
    "treesitter",
    "lsp",
}

-- Install packages
use 'wbthomason/packer.nvim'

use 'windwp/nvim-spectre'                         -- Search/Replace project wide

use 'sheerun/vim-polyglot'                            -- Basic vim support for multiple languages see https://github.com/sheerun/vim-polyglot for the full list.
use 'Glench/Vim-Jinja2-Syntax'                        -- Jinja2 syntax
use 'ziglang/zig.vim'                                 -- Best language ever ?
use 'rust-lang/rust.vim'                              -- Haskell on LLVM ?
use 'simrat39/rust-tools.nvim'
use 'fladson/vim-kitty'
use 'pbrisbin/vim-mkdir'                              -- Save files and create not existing directories
use 'tpope/vim-commentary'
use 'tpope/vim-surround'                              -- Now you can command your surroundings
use 'tpope/vim-fugitive'                              -- Best Vim Git client
use 'junegunn/gv.vim'                                 -- Git diff split
use 'cohama/agit.vim'                                 -- Git log viewer
use 'junegunn/vim-easy-align'                         -- Align text with ease
use 'jansedivy/jai.vim'
use 'j-hui/fidget.nvim'
use "lewis6991/gitsigns.nvim"
use 'junegunn/goyo.vim'
use {
    "folke/todo-comments.nvim",
    requires = "nvim-lua/plenary.nvim",
}

plugin_manager.install()

vim.g.netrw_banner = false
vim.g.netrw_winsize = 25
vim.g.netrw_browse_split = false

vim.cmd [[
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
autocmd FileType yaml setlocal cursorcolumn
]]

vim.cmd [[ nnoremap <expr><CR> {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]]

require"fidget".setup{}

require('gitsigns').setup()

require"todo-comments".setup()
