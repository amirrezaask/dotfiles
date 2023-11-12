local color_state = 'dark'
local dark_colorscheme = 'catppuccin-macchiato'
local light_colorscheme = 'rose-pine-dawn'

vim.cmd.colorscheme(dark_colorscheme)
function ToggleColorscheme()
	if color_state == 'dark' then
		color_state = 'light'
		vim.cmd.colorscheme(light_colorscheme)
	else
		color_state = 'light'
		vim.cmd.colorscheme(dark_colorscheme)
	end
end
