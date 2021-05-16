vim.cmd([[packadd packer.nvim]])
local personal_plugins_path = os.getenv('HOME') .. '/src/github.com/'

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
          opts = { base .. path }
        end
        _use(opts)
      else
        _use(opts)
      end
    end
    -- TODO(amirreza): Some treesitter R&D

    -- Plugin Manager
    use({ 'wbthomason/packer.nvim' })

    -- Colorschemes {{{
    use { 'joshdick/onedark.vim' }
    use { 'mhartington/oceanic-next' }
    use { 'cocopon/iceberg.vim' }
    use { 'AlessandroYorba/Alduin' }
    use { 'tyrannicaltoucan/vim-deep-space' }
    use { 'wadackel/vim-dogrun' }
    use { 'sonph/onehalf' }
    use { 'fcpg/vim-orbital' }
    use { 'lifepillar/vim-solarized8' }
    use { 'liuchengxu/space-vim-dark' }
    use { 'jaredgorski/SpaceCamp' }
    use { 'jacoborus/tender.vim' }
    use { 'rakr/vim-two-firewatch' }
    use { 'gruvbox-community/gruvbox' }
    use { 'amirrezaask/base16.nvim' }
    use { 'tjdevries/colorbuddy.nvim' }
    use { 'amirrezaask/gruvbuddy.nvim' }
    -- }}}

    use { 'norcalli/ui.nvim' }

    -- FZF {{{
    use({ 'junegunn/fzf'})
    use({ 'junegunn/fzf.vim'})
    -- }}}

    -- Fuzzy.nvim {{{
    use { 'amirrezaask/spawn.nvim' }
    use { 'amirrezaask/fuzzy.nvim' }
    -- }}}

    -- Telescope.nvim {{{
    use({ 'nvim-lua/plenary.nvim' })
    use({ 'nvim-lua/popup.nvim' })
    use('nvim-telescope/telescope.nvim')
    use({ 'nvim-telescope/telescope-fzy-native.nvim' })
    use({ 'nvim-telescope/telescope-dap.nvim' })
    use({ 'nvim-telescope/telescope-media-files.nvim' })
    use({ 'nvim-telescope/telescope-github.nvim' })
    use({ 'nvim-telescope/telescope-snippets.nvim' })
    use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }
    -- }}}

    -- Snippets {{{
    use({ 'norcalli/snippets.nvim' })
    -- }}}

    -- Lua {{{
    use({ 'tjdevries/nlua.nvim' })
    -- }}}

    -- Vim Surround text objects
    use({ 'tpope/vim-surround' })

    -- Vim JSON tools
    use({ 'tpope/vim-jdaddy', ft = 'json' })
    use { 'elzr/vim-json' }

    -- Git Integration {{{
    use({ 'tpope/vim-fugitive' })
    use({ 'lewis6991/gitsigns.nvim' })
    use({ 'ThePrimeagen/git-worktree.nvim' })
    use { 'amirrezaask/blame.nvim', requires = {{"nvim-lua/plenary.nvim"}}}
    -- }}}

    -- Comment codes at ease
    use({ 'tpope/vim-commentary' })

    -- LSP {{{
    use({ 'neovim/nvim-lspconfig' })
    use({ 'glepnir/lspsaga.nvim' })
    use({ 'nvim-lua/lsp_extensions.nvim' })
    -- }}}

    use({
      'norcalli/nvim-colorizer.lua',
      branch = 'color-editor',
    })

    -- Languages {{{
    use({ 'honza/dockerfile.vim' })
    use({ 'hashivim/vim-terraform' })
    use({ 'LnL7/vim-nix', ft = 'nix' })
    use({ 'ziglang/zig.vim', ft = 'zig' })
    use({ 'dag/vim-fish', ft = 'fish' })
    use({ 'cespare/vim-toml' })
    use({ 'chr4/nginx.vim' })
    use({ 'elixir-editors/vim-elixir' })
    -- }}}

    -- Completion {{{
    use('hrsh7th/nvim-compe')
    -- }}}

    -- Rust {{{
    use({ 'rust-lang/rust.vim', ft = 'rust' })
    use({ 'simrat39/rust-tools.nvim' })
    -- }}}

    -- Treesitter {{{
    use({ 'nvim-treesitter/nvim-treesitter' })
    use({ 'nvim-treesitter/playground' })
    use({ 'nvim-treesitter/nvim-treesitter-textobjects' })
    use({ 'JoosepAlviste/nvim-ts-context-commentstring' })
    -- }}}

    -- Debugger Adapter Protocol {{{
    use({ 'mfussenegger/nvim-dap' })
    use({ 'theHamsta/nvim-dap-virtual-text' })
    -- }}}

    -- Icons {{{
    use({ 'kyazdani42/nvim-web-devicons' })
    use({ 'yamatsum/nvim-nonicons' }) -- need to have nonicons font installed look into https://github.com/yamatsum/nonicons
    -- }}}
    use {'ThePrimeagen/vim-be-good'}
    use {'ThePrimeagen/harpoon'}
  end,
})
