require('telescope').setup{
    extensions = {
        file_browser = {
          -- disables netrw and use telescope-file-browser in its place
          hijack_netrw = true,
          mappings = {
            ["i"] = {
              -- your custom insert mode mappings
            },
            ["n"] = {
              -- your custom normal mode mappings
            },
          },
        },
    },
}

require"keymaps".bind {
    n = {
        ['<leader><leader>'] = "<cmd>Telescope find_files<CR>",
        ['<leader>h'] = "<cmd>Telescope help_tags<cr>",
        ['<leader>fb'] = "<cmd>Telescope file_browser<CR>",
        ['??'] = '<cmd>Telescope live_grep<CR>',
        ['?a'] = '<cmd>lua vim.lsp.buf.code_action()<CR>',
    }
}
require("telescope").load_extension "file_browser"

require'fuzzy'.setup {
  width = 60,
  height = 40,
  blacklist = {
    "vendor"
  },
  border = 'yes', -- can be 'no' as well
  location = require'fuzzy.lib.location'.bottom_center,
  sorter = require'fuzzy.lib.sorter'.fzy, -- Also fzf_native, fzy_native, string_distance are supported
  prompt = '> ',
  register = {
    some_custom_function = function() -- This function appears in complete menu when using :Fuzzy command.
    end
  }
}
