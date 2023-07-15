require("nvim-treesitter.configs").setup({
	-- Setup treesitter text objects module + highlight
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
require("treesitter-context").setup({ enable = true })
