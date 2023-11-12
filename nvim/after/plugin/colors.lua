local color_state = 'dark'
local dark_colorscheme = 'rose-pine'
local light_colorscheme = 'rose-pine-dawn'

local function colorscheme(name, transparent)
	vim.cmd.colorscheme(name)
	vim.cmd([[ hi LineNr guifg=#5eacd3 ]])
	if transparent then
		vim.cmd([[
		hi Normal guibg=none
		hi NormalNC guibg=none
		hi NormalFloat guibg=none
		hi Visual guibg=#49B9C7 guifg=#F6F6F6
		hi SignColumn guibg=none
	]])
	end
end

function ToggleColorscheme()
	if color_state == 'dark' then
		color_state = 'light'
		colorscheme(light_colorscheme, true)
	else
		color_state = 'light'
		colorscheme(dark_colorscheme, true)
	end
end

colorscheme(dark_colorscheme, false)
