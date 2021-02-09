vim.cmd [[packadd packer.nvim]]
return require'packer'.startup{
   function(use)

     -- Plugin Manager
      use { 'wbthomason/packer.nvim' }

      use { 'amirrezaask/start.nvim' }

      use { 'amirrezaask/fuzzy.nvim' }

      -- Hashicorp tools
      use { 'hashivim/vim-terraform' }

      -- Vim Surround text objects
      use { 'tpope/vim-surround'}
      
      -- Vim JSON tools
      use { 'tpope/vim-jdaddy', ft = 'json' }
     
      -- Elixir
      use { 'elixir-editors/vim-elixir' }

      -- Base16 Colorscheme library
      use { 'norcalli/nvim-base16.lua', branch = 'theme-editor' }
      
      -- Comment codes at ease
      use { 'tpope/vim-commentary' }
      
      -- Neovim builtin LSP configuration
      use { 'neovim/nvim-lspconfig' }

      -- Go
      use { 'fatih/vim-go' }

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
      
      -- Show indents
      use { 'Yggdroot/indentLine' }

      -- Snippets
      use { 'norcalli/snippets.nvim' } -- TODO: please configure this.

      -- Git Integrations
      use { 'rhysd/git-messenger.vim' }
      use { 'kdheepak/lazygit.nvim' }
   end
}
