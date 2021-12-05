local has_nline, nline = pcall(require, "nline")
if not has_nline then
  return
end

local vim = require "nline.parts.vim"
local git = require "nline.parts.git"
local wrappers = require "nline.wrappers"

local me = {
  vim.modified(),
  vim.filename { shorten = false } { hl = "NormalMode" },

  vim.seperator(),

  wrappers.square_brackets(git.branch()),
  wrappers.square_brackets(git.changes()),
  wrappers.square_brackets(vim.line() .. vim.space() .. vim.colon() .. vim.col()),
  vim.filetype(),
}
nline.make(me)
