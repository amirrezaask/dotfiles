require "jasvim.globals"
require "jasvim.plugins"
require "jasvim.options"
require "jasvim.keymaps"
require "jasvim.colorscheme"

-- Enhance neovim user interface using better default handlers for UI
-- and also telescope.nvim
require "jasvim.ui"

-- neovim is a good editor
-- let's make it a great one
require "jasvim.editor"

-- neovim as your IDE
require "jasvim.ide"

-- Programming languages support
require "jasvim.lang"

-- Which key should always load at last to make sure all keymap docs are loaded
require "jasvim.whichkey"
