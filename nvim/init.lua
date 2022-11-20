-- Core
require "jasvim.core.globals"
require "jasvim.core.plugins"
require "jasvim.core.options"
require "jasvim.core.keymaps"
require "jasvim.core.colorscheme"

-- Modules
require "jasvim.modules.git"
require "jasvim.modules.ui"
require "jasvim.modules.mason"
require "jasvim.modules.lsp"
require "jasvim.modules.completion"
require "jasvim.modules.dap"
require "jasvim.modules.telescope"
require "jasvim.modules.treesitter"
require "jasvim.modules.terminal"
require "jasvim.modules.editor"

-- Programming languages
require "jasvim.modules.lang.lua"
require "jasvim.modules.lang.json"
require "jasvim.modules.lang.go"
require "jasvim.modules.lang.php"
require "jasvim.modules.lang.elixir"
require "jasvim.modules.lang.cc"
require "jasvim.modules.lang.rust"
require "jasvim.modules.lang.haskell"
require "jasvim.modules.lang.zig"
require "jasvim.modules.lang.yaml"
require "jasvim.modules.lang.python"

-- Which key should always load at last to make sure all keymap docs are loaded
require "jasvim.modules.whichkey"
