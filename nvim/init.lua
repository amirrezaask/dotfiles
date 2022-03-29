--     ___              _                            ___         __
--    /   |  ____ ___  (_)____________  ____  ____ _/   |  _____/ /__
--   / /| | / __ `__ \/ / ___/ ___/ _ \/_  / / __ `/ /| | / ___/ //_/
--  / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ / ___ |(__  ) ,<
-- /_/  |_/_/ /_/ /_/_/_/  /_/   \___/ /___/\__,_/_/  |_/____/_/|_|
--
-- All configuration of different plugins are in
-- after/plugin/*.lua

-- Everything is just configuration of various plugins

-- Colorscheme stuff
vim.cmd [[ colorscheme sitruuna ]]

-- Basic Keymaps
vim.g.mapleader = " "
local map = function(mode, lhs, rhs) vim.api.nvim_set_keymap(mode, lhs, rhs, {silent=true, noremap=true}) end
map('n', "Q", "<NOP>")
map('n', ';', ':')
map('n', 'q;', 'q:')

map('n', '<Left>', ':vertical resize -5<CR>')
map('n', '<Right>', ':vertical resize +5<CR>')
map('n', '<Up>', ':resize +5<CR>')
map('n', '<Down>', ':resize -5<CR>')

map('n', 'j', 'gj')
map('n', 'k', 'gk')

map('t', '<Esc>', '<C-\\><C-n>')
map('t', 'jk', '<C-\\><C-n>')
map('t', 'kj', '<C-\\><C-n>')

map('i', 'jk', '<esc>')
map('i', 'kj', '<esc>')

map('n', 'Y', 'y$')
map('n', 'n', 'nzz')
map('n', 'N', '"Nzz')

map('n', '<M-t>', ":tabnew<CR>")
map('n', '<M-p>', ":tabprev<CR>")
map('n', '<M-n>', ":tabnext<CR>")

vim.cmd [[ 
  nnoremap <M-j> :m .+1<CR>==
  nnoremap <M-k> :m .-2<CR>==
  inoremap <M-j> <Esc>:m .+1<CR>==gi
  inoremap <M-k> <Esc>:m .-2<CR>==gi
  vnoremap <M-j> :m '>+1<CR>gv=gv
  vnoremap <M-k> :m '<-2<CR>gv=gv
]]

map('n', '{', ':cprev<CR>')
map('n', '}', ':cnext<CR>')

vim.cmd [[ nnoremap <expr><CR> {-> v:hlsearch ? ":nohl<CR>" : "<CR>"}() ]]

vim.opt.smartcase = true -- care about case of chars when we have capital ones in search.
vim.opt.equalalways = false -- don't change windows size after closing one
vim.opt.modeline = true
vim.opt.autoread = true
vim.opt.compatible = false -- no compatibility with vim.
vim.opt.encoding = "utf-8" -- default encoding
vim.opt.hlsearch = true -- highlight matched when searching
vim.opt.history = 700
vim.opt.tabpagemax = 100
vim.opt.ruler = true -- show line/col in statusbar
vim.opt.mouse = "a" -- enable mouse
vim.opt.wrap = true
vim.opt.autoindent = true -- use same indent as previous line
vim.opt.termguicolors = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.backup = false
vim.opt.writebackup = false -- no vim backup file
vim.opt.swapfile = false -- disable vim swap files
vim.opt.splitright = true -- always split window to right
vim.opt.splitbelow = true -- always split to below
vim.opt.cursorline = true -- highlight current line
vim.opt.relativenumber = true -- relative line numbers
vim.opt.number = true -- show current line number
vim.opt.showmode = false
vim.opt.clipboard = "unnamedplus"
vim.opt.hidden = true
vim.opt.updatetime = 100
vim.opt.wildmode = { "longest", "list", "full" }
vim.opt.wildmode = vim.opt.wildmode - "list"
vim.opt.wildmode = vim.opt.wildmode + { "longest", "full" }

-- Plugins
require("packer").startup {
  function(use)
    use { "wbthomason/packer.nvim" } -- Plugin manager
    use { "jghauser/mkdir.nvim", config = function() require "mkdir" end } -- Mkdir
    use { "windwp/nvim-spectre", requires = { "nvim-lua/plenary.nvim" } }
    use { "tpope/vim-surround" } -- Vim surround objects
    use { "neovim/nvim-lspconfig" } -- LSP configurations
    use { "honza/dockerfile.vim" } -- Dockerfile
    use { "hashivim/vim-terraform" } -- Terraform
    use { "LnL7/vim-nix" } -- Nix
    use { "dag/vim-fish" } -- Fish
    use { "cespare/vim-toml" } -- Toml
    use { "elixir-editors/vim-elixir" } -- Elixir
    use { "pearofducks/ansible-vim" } -- Ansible
    use { "Glench/Vim-Jinja2-Syntax" } -- Jinja2
    use { "ziglang/zig.vim" } -- Zig language Support
    use { "hrsh7th/nvim-cmp" } -- completion popup
    use { "hrsh7th/cmp-buffer" } -- source for completion from words in current buffer
    use { "hrsh7th/cmp-nvim-lua" } -- source for completion from neovim stuff
    use { "hrsh7th/cmp-nvim-lsp" } -- source for completion from lsp
    use { "hrsh7th/cmp-path" } -- source for completion from fs path
    use { "rust-lang/rust.vim", ft = "rust" } -- rust syntax
    use { "nvim-treesitter/nvim-treesitter" } -- treesitter integration
    use { "nvim-treesitter/nvim-treesitter-textobjects" } -- more text objects for treesitter
    use { "lukas-reineke/indent-blankline.nvim" } -- Show indent highlights
    use { "fatih/vim-go" } -- Golang IDE
    use { 'fladson/vim-kitty' } 
    use { 'vim-erlang/vim-erlang-runtime' }
    use { 'junegunn/fzf' }
    use { 'junegunn/fzf.vim' }
    use { 'eemed/sitruuna.vim' }
    use { 'luisiacc/gruvbox-baby' }
    use { 'ap/vim-buftabline' }
    use { 'tpope/vim-commentary' }
  end,
}




