require("toggleterm").setup{
  -- size can be a number or function which is passed the current terminal
  size = function(term)
    if term.direction == "horizontal" then
      return 15
    elseif term.direction == "vertical" then
      return vim.o.columns * 0.3
    end
  end,
  open_mapping = [[<C-w>]],
  hide_numbers = true, -- hide the number column in toggleterm buffers
  shade_filetypes = {},
  shade_terminals = true,
  shading_factor = 2, -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
  start_in_insert = true,
  persist_size = true,
  -- direction = 'vertical' | 'horizontal' | 'window' | 'float',
  -- close_on_exit = true, -- close the terminal window when the process exits
  -- shell = vim.o.shell, -- change the default shell
  -- This field is only relevant if direction is set to 'float'
  -- float_opts = {
  --   -- The border key is *almost* the same as 'nvim_win_open'
  --   -- see :h nvim_win_open for details on borders however
  --   -- the 'curved' border is a custom border type
  --   -- not natively supported but implemented in this plugin.
  --   border = 'single' | 'double' | 'shadow' | 'curved' | ... other options supported by win open
  --   width = <value>,
  --   height = <value>,
  --   winblend = 3,
  --   highlights = {
  --     border = "Normal",
  --     background = "Normal",
  --   }
  -- }
}
