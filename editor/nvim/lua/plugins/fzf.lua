return {
	"ibhagwan/fzf-lua",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	config = function()
		local fzfLua = require("fzf-lua")
		fzfLua.setup({
			defaults = {
				previewer = false,
			},
		})
		vim.keymap.set("n", "<leader>f", fzfLua.files, {})
		vim.keymap.set("n", "<leader><leader>", fzfLua.files, {})
		vim.keymap.set("n", "<leader>i", function()
			fzfLua.files({ cwd = vim.fn.stdpath("config"), prompt = "Neovim Config" })
		end, {})

		vim.keymap.set("n", "<C-p>", function()
			fzfLua.git_files({})
		end, {})

		vim.keymap.set("n", "??", fzfLua.grep_project, {})
		vim.keymap.set("n", "<leader>h", fzfLua.help_tags, {})
	end,
}
