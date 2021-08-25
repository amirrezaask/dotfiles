vim.cmd [[packadd packer.nvim]]

local is_wsl = (function()
  local output = vim.fn.systemlist "uname -r"
  return not not string.find(output[1] or "", "WSL")
end)()

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

    -- Base library for my colorscheme {{{
    use { "amirrezaask/palette.nvim" }
    use { "amirrezaask/base16.nvim" }
    use { "eemed/sitruuna.vim" }
    use { "gosukiwi/vim-atom-dark" }
    use { "dracula/vim" }
    use { "overcache/NeoSolarized" }
    use { "sonph/onehalf" }
    use { "chriskempson/base16-vim" }
    -- }}}

    -- Statusline
    use { "amirrezaask/nline.nvim" }

    -- Telescope.nvim {{{
    use { "nvim-lua/popup.nvim" }
    use { "nvim-telescope/telescope.nvim" }
    use { "nvim-telescope/telescope-fzy-native.nvim" }
    use { "nvim-telescope/telescope-dap.nvim" }
    use { "nvim-telescope/telescope-fzf-native.nvim", run = "make" }
    -- }}}

    -- Vim Surround text objects
    use { "tpope/vim-surround" }

    -- Vim JSON tools
    use { "tpope/vim-jdaddy", ft = "json" }
    use { "elzr/vim-json" }

    -- Git Integration {{{
    use { "lewis6991/gitsigns.nvim" }
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

    if not is_wsl then
      use { "yamatsum/nvim-nonicons" }
    end

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

    -- better inc/dec
    use { "monaqa/dial.nvim" }

    -- FZF
    if vim.g.fuzzy_finder == "fzf" then
      use { "junegunn/fzf" }
      use { "junegunn/fzf.vim" }
      use "gfanto/fzf-lsp.nvim"
    end

    use {
      "antoinemadec/FixCursorHold.nvim",
      run = function()
        vim.g.curshold_updatime = 1000
      end,
    }
  end,
}
