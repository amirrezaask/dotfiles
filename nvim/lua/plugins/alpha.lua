local function config()
  local cfg = require"alpha.themes.dashboard".config
  local header = vim.split(
    [[
	    ___              _                               ___         __  
	   /   |  ____ ___  (_)____________  ____  ____ _   /   |  _____/ /__
	  / /| | / __ `__ \/ / ___/ ___/ _ \/_  / / __ `/  / /| | / ___/ //_/
	 / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ /  / ___ |(__  ) ,<   
	/_/  |_/_/ /_/ /_/_/_/  /_/   \___/ /___/\__,_/  /_/  |_/____/_/|_|  
									     
	]],
    "\n"
  )
  cfg.layout[2].val = header
  require("alpha").setup(cfg)
end

return {
  -- "goolord/alpha-nvim",
  -- dependencies = { "nvim-tree/nvim-web-devicons" },
  -- config = config,
}
