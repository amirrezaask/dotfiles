vim.cmd([[packadd packer.nvim]])
return require('packer').startup({
  function(_use)
    local function use(opts)
      local base = '/home/amirreza/src/github.com/'
      local path
      if type(opts) == 'string' then
        path = opts
      else
        path = opts[1]
      end
      if vim.fn.isdirectory(base .. path) ~= 0 then
        opts[1] = base .. path
        _use(opts)
      else
        _use(opts)
      end
    end
    -- Plugin Manager
    use({ 'wbthomason/packer.nvim' })

    use({ 'amirrezaask/spawn.nvim' })
    use({ 'amirrezaask/fuzzy.nvim' })

    -- Fuzzy finders
    use('nvim-lua/popup.nvim')
    use('nvim-telescope/telescope.nvim')
    use('nvim-telescope/telescope-fzy-native.nvim')
    use('nvim-telescope/telescope-dap.nvim')
    use({ 'junegunn/fzf' })
    use({ 'junegunn/fzf.vim' })

    -- Hashicorp tools
    use({ 'hashivim/vim-terraform' })

    -- Vim Surround text objects
    use({ 'tpope/vim-surround' })

    -- Vim JSON tools
    use({ 'tpope/vim-jdaddy', ft = 'json' })

    -- Elixir
    use({ 'elixir-editors/vim-elixir' })

    -- Base16 Themes
    use({ 'amirrezaask/nvim-base16.lua' })

    -- Comment codes at ease
    use({ 'tpope/vim-commentary' })

    -- Neovim builtin LSP configuration
    use({ 'neovim/nvim-lspconfig' })
    use({ 'glepnir/lspsaga.nvim' })
    use({ 'nvim-lua/lsp_extensions.nvim' })

    -- Highlight color codes in buffer with their respective colors
    use({
      'norcalli/nvim-colorizer.lua',
      branch = 'color-editor',
      cmd = {
        'ColorizerAttachToBuffer',
        'ColorizerDetachFromBuffer',
        'ColorizerToggle',
        'ColorizerReloadAllBuffers',
      },
    })
    -- Vim indent objects
    -- Support for Dockerfile syntax
    use({ 'honza/dockerfile.vim' })

    -- Support for Nix programming language
    use({ 'LnL7/vim-nix', ft = 'nix' })
    -- Support for Zig programming language
    use({ 'ziglang/zig.vim', ft = 'zig' })
    -- Support for fish syntax
    use({ 'dag/vim-fish', ft = 'fish' })
    -- Support for Rustlang syntax
    use({ 'rust-lang/rust.vim', ft = 'rust' })
    -- Autocomplete popup
    use('hrsh7th/nvim-compe')

    -- Toml support
    use({ 'cespare/vim-toml' })
    -- Show indents
    use({ 'Yggdroot/indentLine' })

    -- Git Integrations
    use({ 'rhysd/git-messenger.vim' })
    use({
      'lewis6991/gitsigns.nvim',
      requires = {
        'nvim-lua/plenary.nvim',
      },
    })
    use({ 'tjdevries/colorbuddy.nvim' })
    use({ 'amirrezaask/gruvbuddy.nvim' })
    -- Treesitter
    use({ 'nvim-treesitter/nvim-treesitter' })
    use({ 'nvim-treesitter/playground' })

    -- Debugger Adapter Protocol
    use({ 'mfussenegger/nvim-dap' })
    use({ 'theHamsta/nvim-dap-virtual-text' })

    -- Telescope.nvim
    -- Icons
    use('kyazdani42/nvim-web-devicons')
    -- Indent Guides
    use({ 'lukas-reineke/indent-blankline.nvim' })
  end,
})
