local has_nline, nline = pcall(require, "nline")

local vim = require "nline.parts.vim"
local git = require "nline.parts.git"
local lsp = require "nline.parts.lsp"
local wrappers = require "nline.wrappers"

if has_nline then
  nline.make {
    vim.mode(),
    vim.space(),
    vim.modified,
    vim.filename { shorten = true },
    vim.space(),
    "[",
    vim.line(),
    vim.colon(),
    vim.col(),
    "]",
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
