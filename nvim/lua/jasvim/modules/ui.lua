require("dressing").setup()
require("lualine").setup {
  options = {
    icons_enabled = true,
    theme = "auto",
    component_separators = { left = "", right = "" },
    section_separators = { left = "", right = "" },
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {},
    always_divide_middle = true,
    globalstatus = false,
    refresh = {
      statusline = 1000,
      tabline = 1000,
      winbar = 1000,
    },
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch", "diff", "diagnostics" },
    lualine_c = { "filename" },
    lualine_x = { "encoding", "fileformat", "filetype" },
    lualine_y = { "progress" },
    lualine_z = { "location" },
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { "filename" },
    lualine_x = { "location" },
    lualine_y = {},
    lualine_z = {},
  },
  tabline = {},
  winbar = {},
  inactive_winbar = {},
  extensions = {},
}

if vim.version().major >= 0 and vim.version().minor >= 8 then
  vim.opt.laststatus = 3
end

local asciiarts = {
  vim.split(
    [[
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣾⣦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣴⣿⣿⣿⣿⣦⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⡿⢿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⢀⣠⣤⣶⣶⣷⣶⣦⣤⣀⠘⢿⣿⡇⢸⣿⡿⠃⣀⣤⣴⣶⣶⣶⣶⣤⣄⡀⠀
⠀⠈⢿⣿⣿⣿⣿⣿⡿⣿⣿⣷⣄⠘⠇⠸⠁⣠⣾⣿⣿⢿⣿⣿⣿⣿⣿⡿⠁⠀
⠀⠀⠈⢻⣿⣿⣿⣿⣦⣤⣈⡉⠛⢁⣤⣤⡈⠛⢉⣁⣤⣼⣿⣿⣿⣿⠟⠀⠀⠀
⠀⠀⠀⠀⠉⠛⠿⠿⢿⠿⠋⣉⣀⠸⣿⣿⠇⠸⠿⣿⣿⡿⠿⠿⠛⠁⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⢀⣴⣿⣿⠋⣠⠀⣤⡄⠐⣶⣤⣤⡀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⢀⣴⣿⣿⡿⢁⣴⣿⠀⢿⣿⣦⡘⢿⣿⣿⣦⡀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⣼⣿⣿⣿⣿⣿⣿⡿⠁⠘⣿⣿⣿⣿⣿⣿⣿⣷⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⡿⠁⠀⠀⠈⢿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⢹⣿⠿⠿⠛⠉⠀⠀⠀⠀⠀⠀⠉⠛⠿⠿⣿⡟⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
]],
    "\n"
  ),

  vim.split(
    [[
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡟⠁⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠋⠀⠀⠀⠀⠙⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀⠀⢀⡀⠀⠀⢸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⡿⠟⠛⠉⠉⠈⠉⠙⠛⠿⣧⡀⠀⢸⡇⠀⢀⣼⠿⠛⠋⠉⠉⠉⠉⠛⠻⢿⣿
⣿⣷⡀⠀⠀⠀⠀⠀⢀⠀⠀⠈⠻⣧⣸⣇⣾⠟⠁⠀⠀⡀⠀⠀⠀⠀⠀⢀⣾⣿
⣿⣿⣷⡄⠀⠀⠀⠀⠙⠛⠷⢶⣤⡾⠛⠛⢷⣤⡶⠾⠛⠃⠀⠀⠀⠀⣠⣿⣿⣿
⣿⣿⣿⣿⣶⣤⣀⣀⡀⣀⣴⠶⠿⣇⠀⠀⣸⣇⣀⠀⠀⢀⣀⣀⣤⣾⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⡿⠋⠀⠀⣴⠟⣿⠛⢻⣯⠉⠛⠛⢿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⡿⠋⠀⠀⢀⡾⠋⠀⣿⡀⠀⠙⢧⡀⠀⠀⠙⢿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⠃⠀⠀⠀⠀⠀⠀⢀⣾⣧⠀⠀⠀⠀⠀⠀⠀⠈⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⢀⣾⣿⣿⣷⡀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⡆⠀⣀⣀⣤⣶⣿⣿⣿⣿⣿⣿⣶⣤⣀⣀⠀⢠⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
  ]],
    "\n"
  ),
}
math.randomseed(os.clock() * 100000000000)
local button = require("alpha.themes.dashboard").button
local myconfig = {
  layout = {
    { type = "padding", val = 2 },
    {
      type = "text",
      val = asciiarts[math.ceil(math.random(1, 100) % 2) + 1],
      opts = { position = "center", hl = "Type" },
    },
    { type = "padding", val = 2 },
    { type = "text", val = "JasVim", opts = { position = "center", hl = "Type" } },
    { type = "padding", val = 1 },
    { type = "text", val = "0.1", opts = { position = "center", hl = "Type" } },
    { type = "padding", val = 2 },
    {
      type = "group",
      val = {
        button("SPC SPC", "  Find file"),
        button("SPC f g", "  Git files"),
        button("??", "  Live Grep"),
        button("SPC f r", "  Recent Files"),
        button("SPC p s", "  Sync Packages", "<cmd>PackerSync<Cr>"),
      },
      opts = { spacing = 1, hl = "Include" },
    },
    { type = "padding", val = 2 },
    {
      type = "text",
      val = string.format("Neovim version %s.%s", vim.version().major, vim.version().minor),
      opts = { position = "center", hl = "Type" },
    },
  },
}
require("alpha").setup(myconfig)

bind {
  n = {
    [",z"] = { "<cmd>MaximizerToggle<cr>", desc = "Toggle zoom on current window" },
  },
}

require("nvim-tree").setup()
bind {
  n = {
    ['<leader>l'] = '<cmd>NvimTreeToggle<CR>'
  }
}
