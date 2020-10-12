vim.cmd [[packadd packer.nvim]]

local local_plugins_path = '~/w/dotnvim'


return require'packer'.startup{
   function(use)
      local local_use = function (plug_path)
         if vim.fn.isdirectory(vim.fn.expand(local_plugins_path .. plug_path)) == 1 then
            use(local_plugins_path .. plug_path)
         else
            use('amirrezaask/' .. plug_path)
         end
      end
      -- My plugins 
      local_use 'ngo.nvim'
      local_use 'start.nvim'
      local_use 'fuzzy.nvim'
    
      use 'patstockwell/vim-monokai-tasty'
      use { 'lambdalisue/fern.vim' }
      use { 'liuchengxu/vim-which-key' }
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
      -- use { 'tpope/vim-fugitive' }
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
      use { 'nvim-treesitter/nvim-treesitter' }
      use { 'nvim-lua/popup.nvim'}
      use { 'tjdevries/nlua.nvim'}
      use { 'euclidianAce/BetterLua.vim' }
      use { 'nvim-lua/plenary.nvim' }
      use { 'tjdevries/express_line.nvim'}
      use { 'junegunn/goyo.vim' }
   end
}
