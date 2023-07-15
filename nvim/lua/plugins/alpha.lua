local function config()
  local cfg = {
    layout = {
      { type = "padding", val = 2 },
      {
        type = "text",
        val = {},
        opts = {
          position = "center",
          hl = "Type",
        },
      },
      { type = "padding", val = 2 },
    },
    opts = {
      margin = 5,
    },
  }

  cfg.layout[2].val = vim.split(
    [[
	    ___              _                               ___         __  
	   /   |  ____ ___  (_)____________  ____  ____ _   /   |  _____/ /__
	  / /| | / __ `__ \/ / ___/ ___/ _ \/_  / / __ `/  / /| | / ___/ //_/
	 / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ /  / ___ |(__  ) ,<   
	/_/  |_/_/ /_/ /_/_/_/  /_/   \___/ /___/\__,_/  /_/  |_/____/_/|_|  
									     
	]],
    "\n"
  )
  require("alpha").setup(cfg)
end

return {
  "goolord/alpha-nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = config,
}
