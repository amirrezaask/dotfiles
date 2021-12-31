-- Colorscheme
vim.cmd [[ colorscheme gruvbuddy ]]

-- Statusline
local has_nline, nline = pcall(require, "nline")
if not has_nline then
  return
end

local vimparts = require "nline.parts.vim"
local git = require "nline.parts.git"
local wrappers = require "nline.wrappers"
local icons = require "nline.parts.icons"

local tj = {
  vimparts.mode {
    texts = {
      normal = "Normal",
      visual = "Visual",
      visual_block = "VisualBlock",
      insert = "Insert",
      insert_complete = "IComplete",
      command = "Command",
      terminal = "Terminal",
    },
  },
  vimparts.space(),
  icons.git_branch,
  vimparts.space(),
  git.branch(),

  vimparts.seperator(),

  icons.file,
  vimparts.space(),
  vimparts.filename { shorten = true },
  vimparts.modified(),

  vimparts.seperator(),

  wrappers.square_brackets(git.changes()),
  wrappers.square_brackets(vimparts.line() .. vimparts.space() .. vimparts.colon() .. vimparts.col()),
  vimparts.filetype(),
}

nline.make(tj)

