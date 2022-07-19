-- Install packages
require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'

    -- themes
    use 'folke/tokyonight.nvim'
    use 'eemed/sitruuna.vim'                              -- Best Minimal Colorscheme if you like black,yellow and green colors
    use 'joshdick/onedark.vim'
    use 'gruvbox-community/gruvbox'                       -- Popular gruvbox
    use 'sainnhe/sonokai'
    use 'dracula/vim'
    use 'bluz71/vim-nightfly-guicolors'
    use 'projekt0n/github-nvim-theme'

    use 'windwp/nvim-spectre'                         -- Search/Replace project wide
    use 'nvim-treesitter/nvim-treesitter'             -- Treesitter syntax highlighting
    use 'nvim-treesitter/nvim-treesitter-textobjects' -- Treesitter text objects
    use 'hrsh7th/nvim-cmp'                            -- Neovim auto complete menu
    use 'hrsh7th/cmp-buffer'                          -- auto complete buffer source
    use 'hrsh7th/cmp-nvim-lua'                        -- auto complete nvim lua stuff source
    use 'hrsh7th/cmp-nvim-lsp'                        -- auto complete lsp source
    use 'hrsh7th/cmp-path'                            -- auto complete os path source
    use 'neovim/nvim-lspconfig'                       -- LSP client configurations
    use 'nvim-telescope/telescope.nvim'               -- Telescope fuzzy finder by great TJDevries
    use 'nvim-lua/plenary.nvim'                       -- Neovim stdlib lua by TJDevries
    use 'kyazdani42/nvim-web-devicons'
    use 'hrsh7th/vim-vsnip'                               -- Snippets
    use 'junegunn/fzf'                                    -- Google of the command line
    use 'junegunn/fzf.vim'                                -- Integrate fzf into vim as commands
    use 'sheerun/vim-polyglot'                            -- Basic vim support for multiple languages see https://github.com/sheerun/vim-polyglot for the full list.
    use 'Glench/Vim-Jinja2-Syntax'                        -- Jinja2 syntax
    use 'ziglang/zig.vim'                                 -- Best language ever ?
    use 'rust-lang/rust.vim'                              -- Haskell on LLVM ?
    use 'fladson/vim-kitty'                               -- Best Terminal Emulator config syntax
    use 'pbrisbin/vim-mkdir'                              -- Save files and create not existing directories
    use 'tpope/vim-commentary'                            -- Best commenting plugin ever
    use 'tpope/vim-surround'                              -- Now you can command your surroundings
    use 'tpope/vim-fugitive'                              -- Best Vim Git client
    use 'junegunn/gv.vim'                                 -- Git diff split
    use 'cohama/agit.vim'                                 -- Git log viewer
    use 'junegunn/vim-easy-align'                         -- Align text with ease
end)

vim.g.tokyonight_style = "night"
vim.cmd [[ colorscheme sitruuna ]]

vim.cmd [[
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

let g:netrw_browse_split = 0
let g:netrw_banner = 0
let g:netrw_winsize = 25

" Golang
let g:go_fmt_autosave = 1
let g:go_imports_autosave = 1

" Yaml
autocmd FileType yaml setlocal cursorcolumn

" Zig

" Easy Align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign) 
]]

vim.g.zig_fmt_autosave = 1
