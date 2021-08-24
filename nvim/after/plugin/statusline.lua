local has_nline, nline = pcall(require, "nline")

local vim = require "nline.parts.vim"
local git = require "nline.parts.git"
local lsp = require "nline.parts.lsp"
local wrappers = require "nline.wrappers"

if has_nline then
  nline.make {
    vim.modified,
    vim.filename { shorten = false },
    vim.space(),
    vim.seperator(),
    vim.filetype(),
    wrappers.square_brackets(lsp.current_function()),
    wrappers.square_brackets(lsp.diagnostics()),
    vim.space(),
    wrappers.square_brackets(git.branch()),
    vim.space(),
    wrappers.square_brackets(git.changes()),
  }
end
