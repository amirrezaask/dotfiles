local ensure_packer = function()
	local fn = vim.fn
	local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
	if fn.empty(fn.glob(install_path)) > 0 then
		fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
		vim.cmd([[packadd packer.nvim]])
		return true
	end
	return false
end

local packer_bootstrap = ensure_packer()

require("packer").startup(function(use)
	use("wbthomason/packer.nvim")
	use({ "rose-pine/neovim", as = "rose-pine" })
	use({ "folke/tokyonight.nvim" })
	use({ "ellisonleao/gruvbox.nvim" })
	use({ "catppuccin/nvim", as = "catppuccin" })

	use({ "goolord/alpha-nvim", requires = { "nvim-tree/nvim-web-devicons" } })

	use({ "nvim-lualine/lualine.nvim", requires = { "nvim-tree/nvim-web-devicons" } })

	-- Treesitter syntax highlighting and text objects.
	use({
		"nvim-treesitter/nvim-treesitter",
		requires = {
			"nvim-treesitter/nvim-treesitter-textobjects",
			"nvim-treesitter/playground",
			"nvim-treesitter/nvim-treesitter-context",
		},
	})
	-- Editor stuff
	use("tpope/vim-surround") -- surrounding text objects
	use("tpope/vim-abolish") -- useful text stuff
	use("numToStr/Comment.nvim") -- Comment stuff like a boss
	use("tpope/vim-fugitive")
	use("fladson/vim-kitty") -- Support Kitty terminal config syntax
	use("towolf/vim-helm") -- Support for helm template syntax
	use("jansedivy/jai.vim") -- Jai from Jonathan Blow
	use("tpope/vim-sleuth") -- Heuristically set buffer options
	use("lewis6991/gitsigns.nvim")
	use({
		"hrsh7th/nvim-cmp",
		requires = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-vsnip",
			"hrsh7th/vim-vsnip",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-buffer",
		},
	})
	use({
		"neovim/nvim-lspconfig",
		requires = { "williamboman/mason.nvim" },
	})

	use({
		"mfussenegger/nvim-dap",
		requires = {
			"rcarriga/nvim-dap-ui",
			"leoluz/nvim-dap-go",
			"theHamsta/nvim-dap-virtual-text",
		},
	})
	use("pbrisbin/vim-mkdir") -- Automatically create directory if not exists
	use("tpope/vim-eunuch") -- Helper commands like :Rename, :Move, :Delete, :Remove, ...
	use({
		"nvim-telescope/telescope.nvim",
		requires = {
			"nvim-lua/plenary.nvim",
			{ "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
			"nvim-telescope/telescope-ui-select.nvim",
		},
	})
	use("fatih/vim-go")
	if packer_bootstrap then
		require("packer").sync()
	end
end)

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end

function get_path_sep()
	if vim.fn.has("win32") == 1 then
		return "\\"
	else
		return "/"
	end
end
