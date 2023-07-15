local function config()
	local cfg = require'alpha.themes.dashboard'.config
	cfg.layout[2].val = vim.split([[
	    ___              _                               ___         __  
	   /   |  ____ ___  (_)____________  ____  ____ _   /   |  _____/ /__
	  / /| | / __ `__ \/ / ___/ ___/ _ \/_  / / __ `/  / /| | / ___/ //_/
	 / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ /  / ___ |(__  ) ,<   
	/_/  |_/_/ /_/ /_/_/_/  /_/   \___/ /___/\__,_/  /_/  |_/____/_/|_|  
									     
	]], "\n")
	table.remove(cfg.layout, 4)
	require'alpha'.setup(cfg)
end

return {
 "goolord/alpha-nvim", dependencies = { "nvim-tree/nvim-web-devicons" }, config = config 
}
