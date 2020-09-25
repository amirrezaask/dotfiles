vim.cmd [[packadd packer.nvim]]

return require'packer'.startup(function()
    use { 'airblade/vim-gitgutter'}
    use { 'rafi/awesome-vim-colorschemes'}
    use { 'dracula/vim', as = 'dracula' }
    use { 'overcache/NeoSolarized'}
    use { 'whatyouhide/vim-gotham'}
    use { 'arcticicestudio/nord-vim'}
    use { 'gosukiwi/vim-atom-dark'}
    use { 'morhetz/gruvbox'}
    use { 'tpope/vim-surround'}
    use { 'tpope/vim-jdaddy'}
    use { 'tpope/vim-commentary' }
    use { 'tpope/vim-fugitive' }
    use { 'lambdalisue/fern.vim'}
    use { 'neovim/nvim-lspconfig'}
    use { 'vimlab/split-term.vim'}
    use { 'junegunn/fzf.vim'}
    use { 'junegunn/fzf'}
    use { 'nathanaelkane/vim-indent-guides'}
    use { 'michaeljsmith/vim-indent-object'}
    use { 'honza/dockerfile.vim'}
    use { 'LnL7/vim-nix'}
    use { 'ziglang/zig.vim'}
    use { 'dag/vim-fish'}
    use { 'rust-lang/rust.vim'}
    use { 'fatih/vim-go' }
    use { 'nvim-lua/completion-nvim'}
    use { 'tjdevries/nlua.nvim'}
    use { 'nvim-lua/plenary.nvim'}
    -- use { 'tjdevries/express_line.nvim'}
    use { 'mhinz/vim-startify' }
    use { 'itchyny/lightline.vim' }
end)

