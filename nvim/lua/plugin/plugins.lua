vim.cmd([[packadd packer.nvim]])
local personal_plugins_path = os.getenv('GOPATH') .. '/github.com/amirrezaask'

return require('packer').startup({
  function(_use)
    local function use(opts)
      local base = personal_plugins_path 
      local path
      if type(opts) == 'string' then
        path = opts
      else
        path = opts[1]
      end
      if vim.fn.isdirectory(base .. path) ~= 0 then
        if type(opts) == 'table' then
          opts[1] = base .. path
        elseif type(opts) == 'string' then
          opts = {base..path}
        end
        _use(opts)
      else
        _use(opts)
      end
    end
    -- TODO(amirreza): simple function? in to put selected lines in a block
    -- TODO(amirreza): Some treesitter R&D
    -- Plugin Manager
    use({ 'wbthomason/packer.nvim' })

    -- Ultimate Colorscheme of all time
    use({ 'gruvbox-community/gruvbox' })
    use({ 'amirrezaask/nvim-base16.lua' })
    use({ 'tjdevries/colorbuddy.nvim' })
    use({ 'amirrezaask/gruvbuddy.nvim' })

    -- Telescope.nvim
    use({ 'nvim-lua/plenary.nvim' })
    use({'nvim-lua/popup.nvim'})
    use({'nvim-lua/completion.nvim'})

    -- use('nvim-telescope/telescope.nvim')
    use({ 'amirrezaask/telescope.nvim' })
    use({'nvim-telescope/telescope-fzy-native.nvim'})
    use({'nvim-telescope/telescope-dap.nvim'})
    use({'nvim-telescope/telescope-media-files.nvim'})
    use({ 'ThePrimeagen/git-worktree.nvim' })
    use({ 'norcalli/snippets.nvim' })

    -- nlua for dev
    use({ 'tjdevries/nlua.nvim' })

    -- Hashicorp tools
    use({ 'hashivim/vim-terraform' })

    -- Vim Surround text objects
    use({ 'tpope/vim-surround' })

    -- Vim JSON tools
    use({ 'tpope/vim-jdaddy', ft = 'json' })

    -- Elixir
    use({ 'elixir-editors/vim-elixir' })

    -- Git Integration [[
    use({ 'tpope/vim-fugitive' })
    use({ 'rhysd/git-messenger.vim' })
    use({ 'lewis6991/gitsigns.nvim' })
    -- ]]

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
    })
   
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
    use({ 'simrat39/rust-tools.nvim' })
    
    -- Autocomplete popup
    use('hrsh7th/nvim-compe')

    use({ 'chr4/nginx.vim' })
   
    -- Toml support
    use({ 'cespare/vim-toml' })
   
    -- Show indents
    use({ 'Yggdroot/indentLine' })

    -- Treesitter
    use({ 'nvim-treesitter/nvim-treesitter' })
    use({ 'nvim-treesitter/playground' })
    use({ 'JoosepAlviste/nvim-ts-context-commentstring' })

    -- Debugger Adapter Protocol
    use({ 'mfussenegger/nvim-dap' })
    use({ 'theHamsta/nvim-dap-virtual-text' })

    -- Icons
    use({'kyazdani42/nvim-web-devicons'})
    use({ 'kyamatsum/nvim-nonicons' }) -- need to have nonicons font installed look into https://github.com/yamatsum/nonicons

    -- Indent Guides
    use({ 'lukas-reineke/indent-blankline.nvim' })
  end,
})
