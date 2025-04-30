vim.g.mapleader = " "
vim.g.maplocalleader = ","

function Missing(mod)
  local ok, _ = pcall(require, mod)
  return not ok
end

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
local o = vim.o

o.rtp = o.rtp .. "," .. lazypath

require("lazy").setup({
  "amirrezaask/nvim-gruvbuddy.lua", -- Colorscheme, inspired by great @tjdevries.
  "nvim-tree/nvim-web-devicons", -- Icons in terminal, nice.

  "supermaven-inc/supermaven-nvim", -- AI Apocalypse
  "MagicDuck/grug-far.nvim", -- Find/Replace project wide.
  "stevearc/oil.nvim", -- File manager done right.
  "williamboman/mason.nvim", -- Package manager for your system inside neovim.
  { "saghen/blink.cmp", tag = "v1.1.1" }, -- Blazingly fast autocomplete
  "stevearc/conform.nvim", -- Autoformat/fixes
  "folke/snacks.nvim", -- Plugin collection but i use the picker only.
  { "ibhagwan/fzf-lua", enabled = false }, -- as a fallback for snacks picker.

  "lewis6991/gitsigns.nvim", -- Git signs
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
  },

  "folke/lazydev.nvim", -- Better neovim development support.
})

--- :h runtimepath
--- First this file $MYVIMRC is sourced.
--- Then the /plugin files are loaded.
--- Then the /after/plugin files are loaded.
--- On Opening a file with Filetype lua, the corresponding /after/ftplugin file is loaded.
--- In this file we just install our plugins, all plugins have a corresponding after/plugin/ file.
