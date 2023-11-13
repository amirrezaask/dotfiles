local color_state = 'dark'
local dark_colorscheme = 'catppuccin'
local light_colorscheme = 'rose-pine-dawn'

function Color(name, transparent)
	vim.cmd.colorscheme(name)
	vim.cmd([[ hi LineNr guifg=#5eacd3 ]])
	if transparent then
		vim.cmd([[
		hi Normal guibg=none
		hi NormalNC guibg=none
		hi NormalFloat guibg=none
		hi SignColumn guibg=none
	]])
	end
end

function ToggleColorscheme()
	if color_state == 'dark' then
		color_state = 'light'
		Color(light_colorscheme, true)
	else
		color_state = 'light'
		Color(dark_colorscheme, true)
	end
end

Color(dark_colorscheme, true)
