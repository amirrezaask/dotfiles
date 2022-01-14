--     ___              _                            ___         __
--    /   |  ____ ___  (_)____________  ____  ____ _/   |  _____/ /__
--   / /| | / __ `__ \/ / ___/ ___/ _ \/_  / / __ `/ /| | / ___/ //_/
--  / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ / ___ |(__  ) ,<
-- /_/  |_/_/ /_/ /_/_/_/  /_/   \___/ /___/\__,_/_/  |_/____/_/|_|
--
-- Configuration is splitted into modules
-- Modules are in lua/amirrezaask namespace
--

-- Install package manage
require "amirrezaask.plugins"

-- Basic vim options
require "amirrezaask.options"

-- UI 
require "amirrezaask.ui"

-- Editor stuff
require "amirrezaask.editor"

-- Git
require "amirrezaask.git"

-- Telescope
require "amirrezaask.telescope"

-- LSP
require "amirrezaask.lsp"

-- IDE like actions
require "amirrezaask.actions"

-- Auto complete
require "amirrezaask.autocomplete"

-- Treesitter
require "amirrezaask.treesitter"

-- Terminal
require "amirrezaask.terminal"

-- Golang
require "amirrezaask.go"
