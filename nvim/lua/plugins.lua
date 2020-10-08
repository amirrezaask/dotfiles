vim.cmd [[packadd packer.nvim]]


return require'packer'.startup(function()
    use { 'lambdalisue/fern.vim' }
    use { '~/w/dotnvim/ngo.nvim' }
    use { '~/w/dotnvim/fuzzy.nvim' }
    use { 'wbthomason/packer.nvim' }
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
    use { 'neovim/nvim-lspconfig'}
    use { 'vimlab/split-term.vim'}
    use { 'nathanaelkane/vim-indent-guides'}
    use { 'michaeljsmith/vim-indent-object'}
    use { 'honza/dockerfile.vim'}
    use { 'LnL7/vim-nix'}
    use { 'ziglang/zig.vim'}
    use { 'dag/vim-fish'}
    use { 'tjdevries/colorbuddy.vim' }
    use { 'tjdevries/gruvbuddy.nvim' }
    use { 'rust-lang/rust.vim'}
    use { 'nvim-lua/completion-nvim'}
    use { 'nvim-lua/popup.nvim'}
    use { 'tjdevries/nlua.nvim'}
    use { 'euclidianAce/BetterLua.vim' }
    use { 'nvim-lua/plenary.nvim' }
    use { 'tjdevries/express_line.nvim'}
    use { 'mhinz/vim-startify' }
    use { 'junegunn/goyo.vim' }
end)
