-- Colorscheme
vim.cmd [[ colorscheme dracula ]]

-- Statusline
local has_nline, nline = pcall(require, "nline")
if not has_nline then
  return
end

vim.cmd [[ hi Normal guibg=None ]]

local vimparts = require "nline.parts.vim"
local git = require "nline.parts.git"
local wrappers = require "nline.wrappers"

local simple = {
  vimparts.space(),
  vimparts.filename { shorten = false },

  vimparts.space(),
  vimparts.pipe(),
  vimparts.space(),

  git.branch(),

  vimparts.seperator(),


  vimparts.space(),
  vimparts.pipe(),
  vimparts.space(),

vimparts.line() .. vimparts.space() .. vimparts.colon() .. vimparts.col(),

  vimparts.space(),
  vimparts.pipe(),
  vimparts.space(),

  git.changes(),
  vimparts.space(),
  vimparts.pipe(),
  vimparts.space(),

  vimparts.filetype(),
}

nline.make(simple)

