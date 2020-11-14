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

      -- Plugin Manager
      use { 'wbthomason/packer.nvim' }

      -- My plugins 
      local_use 'ngo.nvim'
      local_use 'music_player.nvim'
      local_use 'Fuzzy.nvim'
      
      -- Vim surrounding text objects
      use { 'tpope/vim-surround'}
      
      -- Vim JSON tools
      use { 'tpope/vim-jdaddy', ft = 'json' }
      
      -- Base16 Colorscheme library
      use { 'norcalli/nvim-base16.lua', branch = 'theme-editor' }
      
      -- Comment codes at ease
      use { 'tpope/vim-commentary' }
      
      -- Neovim builtin LSP configuration
      use { 'neovim/nvim-lspconfig' }
     
      -- Highlight color codes in buffer with their respective colors
      use { 'norcalli/nvim-colorizer.lua', branch = 'color-editor', cmd = {'ColorizerAttachToBuffer', 'ColorizerDetachFromBuffer', 'ColorizerToggle', 'ColorizerReloadAllBuffers'}}
     
      -- Vim indent objects
      use { 'michaeljsmith/vim-indent-object'}
      
      -- Support for Dockerfile syntax
      use { 'honza/dockerfile.vim'}
      
      -- Support for Nix programming language
      use { 'LnL7/vim-nix', ft = 'nix'}
     
      -- Support for Zig programming language 
      use { 'ziglang/zig.vim', ft = 'zig'}
     
      -- Support for fish syntax
      use { 'dag/vim-fish', ft = 'fish'}
      
      -- Support for Rustlang syntax
      use { 'rust-lang/rust.vim', ft='rust'}
     
      -- Autocomplete popup
      use { 'nvim-lua/completion-nvim'}
      use { 'nvim-lua/popup.nvim'}
      use { 'aca/completion-tabnine' }
      
      -- Lua Neovim development environment
      use { 'tjdevries/nlua.nvim'}
      use { 'euclidianAce/BetterLua.vim' }
      use { 'nvim-lua/plenary.nvim' }
      
      -- Centralize the content of the buffer.
      use { 'junegunn/goyo.vim' }

      -- Show indents
      use { 'Yggdroot/indentLine' }

      -- Snippets
      use { 'norcalli/snippets.nvim' }

      -- Treesitter
      use { 'nvim-treesitter/nvim-treesitter' }

      -- Tj color buddy
      use {'tjdevries/colorbuddy.vim'}
      use {'tjdevries/gruvbuddy.nvim'}

      -- Git Integrations
      use { 'rhysd/git-messenger.vim' }
      use { 'kdheepak/lazygit.nvim' }
   end
}
