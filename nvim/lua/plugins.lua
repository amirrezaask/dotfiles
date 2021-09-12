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

local personal_plugins_path = os.getenv "HOME" .. "/src/repos/personal/plugins/"

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
      local name = vim.split(path, "/")[2]
      if vim.fn.isdirectory(base .. name) ~= 0 then
        if type(opts) == "table" then
          opts[1] = base .. name
        elseif type(opts) == "string" then
          opts = { base .. name }
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

    -- Tool that I use for my colorscheme
    use { "amirrezaask/palette.nvim" }

    -- My fork from emmad/sitruuna.vim
    use { "amirrezaask/sitruuna.vim" }

    use { "gruvbox-community/gruvbox" }

    -- Statusline
    use {
      "amirrezaask/nline.nvim",
    }

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

    -- Git changed signs and also git blame
    use {
      "lewis6991/gitsigns.nvim",
      config = function()
        require("gitsigns").setup {
          signs = {
            add = { text = "|", numhl = "GitSignsAddNr" },
            change = { text = "|", numhl = "GitSignsChangeNr" },
            delete = { text = "_", numhl = "GitSignsDeleteNr" },
            topdelete = { text = "‾", numhl = "GitSignsDeleteNr" },
            changedelete = { text = "~-", numhl = "GitSignsChangeNr" },
          },
          numhl = false,
          current_line_blame = false,
          current_line_blame_opts = {
            delay = 800,
            virt_text_pos = "eol",
          },
        }
      end,
    }

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
      config = function()
        function ColorPicker()
          _PICKER_ASHKAN_KIANI_COPYRIGHT_2020_LONG_NAME_HERE_ = nil
          require("colorizer").color_picker_on_cursor()
        end
        vim.cmd [[ autocmd BufEnter * ColorizerAttachToBuffer ]]
        vim.cmd [[ command! ColorPicker lua ColorPicker ]]
      end,
    }

    -- File Explorer
    use {
      "tamago324/lir.nvim",
    }

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
    use "hrsh7th/nvim-cmp"
    use "hrsh7th/cmp-buffer"
    use "hrsh7th/cmp-nvim-lua"
    use "hrsh7th/cmp-nvim-lsp"
    use "hrsh7th/cmp-path"
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
    use {
      "folke/todo-comments.nvim",
      requires = "nvim-lua/plenary.nvim",
      config = function()
        require("todo-comments").setup {}
        vim.map {
          ["<M-e>"] = ":TodoTelescope<CR>",
        }
      end,
    }

    -- beautify text :)
    use { "godlygeek/tabular" }

    -- Nvim Lua dev & docs {{{
    use { "tjdevries/nlua.nvim" }
    use { "milisims/nvim-luaref" }
    use { "nanotee/luv-vimdocs" }
    -- }}}

    -- :Messages
    use "tpope/vim-scriptease"

    -- better inc/dec
    use {
      "monaqa/dial.nvim",
      config = function()
        vim.cmd [[
          nmap <C-a> <Plug>(dial-increment)
          nmap <C-x> <Plug>(dial-decrement)
          vmap <C-a> <Plug>(dial-increment)
          vmap <C-x> <Plug>(dial-decrement)
          vmap g<C-a> <Plug>(dial-increment-additional)
          vmap g<C-x> <Plug>(dial-decrement-additional)
          ]]
      end,
    }

    use {
      "antoinemadec/FixCursorHold.nvim",
      run = function()
        vim.g.curshold_updatime = 1000
      end,
    }
  end,
}
