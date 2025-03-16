if _G.FuzzyFinder then
	vim.keymap.set("n", "<leader>ff", _G.FuzzyFinder.FindFiles)
	vim.keymap.set("n", "<leader><leader>", _G.FuzzyFinder.FindFiles)
	vim.keymap.set("n", "<leader>b", _G.FuzzyFinder.Buffers)
	vim.keymap.set("n", "<leader>h", _G.FuzzyFinder.HelpTags)
	vim.keymap.set("n", "<C-p>", _G.FuzzyFinder.GitFiles)
	vim.keymap.set("n", "??", _G.FuzzyFinder.Grep)
end
