return {
	"nvim-treesitter/nvim-treesitter",
	dependencies = {
		"nvim-treesitter/nvim-treesitter-textobjects",
		"nvim-treesitter/playground",
	},
	config = function()
		require("nvim-treesitter.configs").setup({
			sync_install = false,
			auto_install = true,
			ignore_install = {},
			modules = {},
			ensure_installed = {
				"json",
				"yaml",
				"c",
				"cpp",
				"lua",
				"rust",
				"go",
				"python",
				"php",
				"ocaml",
				"sql",
			},
			context_commentstring = { enable = true },
			highlight = { enable = true, additional_vim_regex_highlighting = false },
			textobjects = {
				select = {
					enable = true,
					lookahead = true,
					keymaps = {
						["af"] = "@function.outer",
						["if"] = "@function.inner",
						["ac"] = "@class.outer",
						["ic"] = "@class.inner",
					},
				},
			},
		})

		-- Install all treesitter parsers.
		pcall(require("nvim-treesitter.install").update({ with_sync = true }))
	end,
}
