local function config()
  local cfg = require"alpha.themes.dashboard".config
  local header = vim.split(
    [[]],
    "\n"
  )
  cfg.layout[2].val = header
  require("alpha").setup(cfg)
end

return {
  "goolord/alpha-nvim",
  -- dependencies = { "nvim-tree/nvim-web-devicons" },
  -- config = config,
}
