vim.cmd [[packadd packer.nvim]]
local local_plugins_path = '~/w/dotnvim/'
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
      local_use 'music_player.nvim'
      local_use 'Fuzzy.nvim'

      use { 'lambdalisue/fern.vim' }
      use { 'wbthomason/packer.nvim' }
      use { "norcalli/nvim-base16.lua" }
      use { 'tpope/vim-surround'}
      use { 'tpope/vim-jdaddy'}
      use { 'tpope/vim-commentary' }
      use { 'neovim/nvim-lspconfig'}
      use { 'vimlab/split-term.vim'}
      use { 'norcalli/nvim-colorizer.lua' }
      use { 'michaeljsmith/vim-indent-object'}
      use { 'honza/dockerfile.vim'}
      use { 'LnL7/vim-nix'}
      use { 'ziglang/zig.vim'}
      use { 'dag/vim-fish'}
      use { 'rust-lang/rust.vim'}
      use { 'nvim-lua/completion-nvim'}
      use { 'joshdick/onedark.vim' }
      use { 'nvim-lua/popup.nvim'}
      use { 'tjdevries/nlua.nvim'}
      use { 'euclidianAce/BetterLua.vim' }
      use { 'nvim-lua/plenary.nvim' }
      use { 'junegunn/goyo.vim' }
      use { 'Yggdroot/indentLine' }
      use { 'nvim-lua/telescope.nvim' }
      use { 'tjdevries/gruvbuddy.nvim' }
      use { 'kyazdani42/nvim-web-devicons' }
      use { 'ryanoasis/vim-devicons' }
      use { 'tjdevries/colorbuddy.vim' }
      use { 'tjdevries/express_line.nvim' }
      use { 'norcalli/snippets.nvim' }
      -- Git Integrations
      use { 'rhysd/git-messenger.vim' }
      use { 'kdheepak/lazygit.nvim' }
   end
}
