
local nline = require"nline"
local vimparts = require "nline.parts.vim"
local git = require "nline.parts.git"

nline.make({
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
})


