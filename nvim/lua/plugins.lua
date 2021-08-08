vim.cmd [[packadd packer.nvim]]

-- Took from TJDevries configuration
if not pcall(require, "packer") then
  if vim.fn.input "Download Packer? (y for yes)" ~= "y" then
    return
  end

  local directory = string.format("%s/site/pack/packer/start/", vim.fn.stdpath "data")

  vim.fn.mkdir(directory, "p")

  local out = vim.fn.system(
    string.format("git clone %s %s", "https://github.com/wbthomason/packer.nvim", directory .. "/packer.nvim")
  )

  print(out)
  print "Downloading packer.nvim..."
  print "( You'll need to restart now )"
end

local personal_plugins_path = os.getenv "HOME" .. "/src/github.com/"

require("packer").startup {
  function(_use)
    local function use(opts)
      local base = personal_plugins_path
      local path
      if type(opts) == "string" then
        path = opts
      else
        path = opts[1]
      end
      if vim.fn.isdirectory(base .. path) ~= 0 then
        if type(opts) == "table" then
          opts[1] = base .. path
        elseif type(opts) == "string" then
          opts = { base .. path }
        end
        _use(opts)
      else
        _use(opts)
      end
    end
    -- Plugin Manager
    use { "wbthomason/packer.nvim" }

    -- Missing stdlib for neovim
    use { "nvim-lua/plenary.nvim" }

    use { "amirrezaask/palette.nvim" }
    -- Telescope.nvim {{{
    use { "nvim-lua/popup.nvim" }
    use { "nvim-telescope/telescope.nvim" }
    use { "nvim-telescope/telescope-fzy-native.nvim" }
    use { "nvim-telescope/telescope-dap.nvim" }
    use { "nvim-telescope/telescope-media-files.nvim" }
    use { "nvim-telescope/telescope-github.nvim" }
    use { "nvim-telescope/telescope-snippets.nvim" }
    use { "nvim-telescope/telescope-fzf-native.nvim", run = "make" }
    -- }}}

    -- Snippets {{{
    use { "norcalli/snippets.nvim" }
    -- }}}

    -- Vim Surround text objects
    use { "tpope/vim-surround" }

    -- Vim JSON tools
    use { "tpope/vim-jdaddy", ft = "json" }
    use { "elzr/vim-json" }

    -- Git Integration {{{
    use { "tpope/vim-fugitive" }
    use { "lewis6991/gitsigns.nvim" }
    use { "ThePrimeagen/git-worktree.nvim" }
    use { "amirrezaask/blame.nvim", requires = { { "nvim-lua/plenary.nvim" } } }

    use { "tanvirtin/vgit.nvim" }
    -- }}}

    -- Comment codes at ease
    use { "tpope/vim-commentary" }

    -- LSP {{{
    use { "neovim/nvim-lspconfig" }
    use { "nvim-lua/lsp_extensions.nvim" }
    use { "onsails/lspkind-nvim" }
    use { "nvim-lua/lsp-status.nvim" }
    -- }}}

    -- Highlight colors
    use {
      "norcalli/nvim-colorizer.lua",
      branch = "color-editor",
    }

    -- File Explorer
    use { "tamago324/lir.nvim" }

    -- Languages {{{
    use { "honza/dockerfile.vim" }
    use { "hashivim/vim-terraform" }
    use { "LnL7/vim-nix" }
    use { "ziglang/zig.vim" }
    use { "dag/vim-fish" }
    use { "cespare/vim-toml" }
    use { "chr4/nginx.vim" }
    use { "elixir-editors/vim-elixir" }
    use { "neovimhaskell/haskell-vim" }
    use { "justinmk/vim-syntax-extra" }
    use { "goodell/vim-mscgen" }
    use { "pearofducks/ansible-vim" }
    use { "PProvost/vim-ps1" }
    use { "Glench/Vim-Jinja2-Syntax" }
    -- }}}

    -- Define same action for different Languages and projects
    use { "amirrezaask/actions.nvim" }

    -- Completion {{{
    use "hrsh7th/nvim-compe"
    -- }}}

    -- Rust {{{
    use { "rust-lang/rust.vim", ft = "rust" }
    use { "simrat39/rust-tools.nvim" }
    -- }}}

    -- Treesitter {{{
    use { "nvim-treesitter/nvim-treesitter" }
    use { "nvim-treesitter/playground" }
    use { "nvim-treesitter/nvim-treesitter-textobjects" }
    use { "JoosepAlviste/nvim-ts-context-commentstring" }
    -- }}}

    -- Debugger Adapter Protocol {{{
    use { "mfussenegger/nvim-dap" }
    use { "theHamsta/nvim-dap-virtual-text" }
    -- }}}

    -- Icons {{{
    use { "kyazdani42/nvim-web-devicons" }
    use { "yamatsum/nvim-nonicons" }

    -- Startscreen also session manager
    use { "mhinz/vim-startify" }

    -- Highlight todo and etc...
    use { "folke/todo-comments.nvim", requires = "nvim-lua/plenary.nvim" }

    -- beautify text :)
    use { "godlygeek/tabular" }

    -- File tree ( added for @mnim220 )
    use { "kyazdani42/nvim-tree.lua" }

    -- Nvim Lua dev & docs {{{
    use { "tjdevries/nlua.nvim" }
    use { "milisims/nvim-luaref" }
    use { "nanotee/luv-vimdocs" }
    -- }}}

    -- :Messages
    use "tpope/vim-scriptease"

    -- Statusline
    use { "amirrezaask/nline.nvim" }

    -- better inc/dec
    use { "monaqa/dial.nvim" }

    -- refactoring plugin
    use { "ThePrimeagen/refactoring.nvim" }
  end,
  config = {
    display = {
      open_fn = require("packer.util").float,
    },
  },
}

-- Load plugin configuration from lua/configs
-- Took from tjdevries/astronauta.nvim
for _, file in ipairs(vim.api.nvim_get_runtime_file("lua/plugin/**/*.lua", true)) do
  local ok, msg = pcall(loadfile(file))
  if not ok then
    print("Failed to load: ", file)
    print("\t", msg)
  end
end
