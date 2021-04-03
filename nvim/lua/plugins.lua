vim.cmd [[packadd packer.nvim]]
return require'packer'.startup{
   function(_use)
      local function use(opts)
       base = "/home/amirreza/src/github.com/"
       path = opts[1]
       if vim.fn.isdirectory(base .. path) ~= 0 then
         opts[1] = base .. path
         _use(opts)
       else
         _use(opts)
       end
      end
      -- Plugin Manager
      use { 'wbthomason/packer.nvim' }
      -- Fuzzy.nvim
      use { 'amirrezaask/fuzzy.nvim' }

      -- Telescope.nvim
      use { 'nvim-lua/plenary.nvim' }
      use { 'nvim-lua/popup.nvim' }
      use { 'nvim-telescope/telescope.nvim' }
      -- Hashicorp tools
      use { 'hashivim/vim-terraform' }

      use { 'mhinz/vim-startify' }

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
      use { 'kabouzeid/nvim-lspinstall' }
      use { 'glepnir/lspsaga.nvim' }

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
      -- Show indents
      use { 'Yggdroot/indentLine' }

      -- Snippets
      use { 'norcalli/snippets.nvim' } -- TODO: please configure this.

      -- Git Integrations
      use { 'rhysd/git-messenger.vim' }
      use { 'kdheepak/lazygit.nvim' }

      -- Colorbuddy
      use { 'tjdevries/colorbuddy.vim' }
      use { 'tjdevries/gruvbuddy.nvim' }

      -- Debugger Adapter Protocol
      use { 'mfussenegger/nvim-dap' }
      use { 'nvim-treesitter/nvim-treesitter' }
      use { 'theHamsta/nvim-dap-virtual-text' }

      -- Statusline
      use { 'kyazdani42/nvim-web-devicons' }
      use { 'tjdevries/express_line.nvim' }

      -- Lua
      use { 'tjdevries/nlua.nvim' }

   end
}
