vim.api.nvim_create_user_command("TTerm", function()
	vim.cmd [[ tabnew | term ]]
end, {})
