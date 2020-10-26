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
      -- local_use 'start.nvim'
      local_use 'music_player.nvim'
      local_use 'Fuzzy.nvim'
      
      -- Side file browser
      use { 'lambdalisue/fern.vim' }
      -- Vim surrounding text objects
      use { 'tpope/vim-surround'}
      -- Vim JSON tools
      use { 'tpope/vim-jdaddy'}
      use { 'norcalli/nvim-base16.lua' }
      -- Comment codes at ease
      use { 'tpope/vim-commentary' }
      -- Neovim builtin LSP configuration
      use { 'neovim/nvim-lspconfig'}
      -- Split Terminal
      use { 'vimlab/split-term.vim'}
      -- Highlight color codes in buffer with their respective colors
      use { 'norcalli/nvim-colorizer.lua' }
      -- Vim indent objects
      use { 'michaeljsmith/vim-indent-object'}
      -- Support for Dockerfile syntax
      use { 'honza/dockerfile.vim'}
      -- Support for Nix programming language
      use { 'LnL7/vim-nix'}
      -- Support for Zig programming language 
      use { 'ziglang/zig.vim'}
      -- Support for fish syntax
      use { 'dag/vim-fish'}
      -- Support for Rustlang syntax
      use { 'rust-lang/rust.vim'}
      -- Autocomplete popup
      use { 'nvim-lua/completion-nvim'}
      use { 'nvim-lua/popup.nvim'}
      -- Lua Neovim development environment
      use { 'tjdevries/nlua.nvim'}
      use { 'euclidianAce/BetterLua.vim' }
      use { 'nvim-lua/plenary.nvim' }
      -- Centralize the content of the buffer.
      use { 'junegunn/goyo.vim' }
      -- Show indents
      use { 'Yggdroot/indentLine' }
      -- Colorschemes
      use { 'tjdevries/colorbuddy.vim' }
      use { 'tjdevries/gruvbuddy.nvim' }
      -- Statusline
      use { 'tjdevries/express_line.nvim' }
      use { 'norcalli/snippets.nvim' }

      -- Git Integrations
      use { 'rhysd/git-messenger.vim' }
      use { 'kdheepak/lazygit.nvim' }
   end
}
