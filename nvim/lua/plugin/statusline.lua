local has_nline, nline = pcall(require, 'nline')
if not has_nline then
  return
end

local vim = require('nline.parts.vim')
local git = require('nline.parts.git')
local lsp = require('nline.parts.lsp')
local icons = require('nline.parts.icons')
local wrappers = require('nline.wrappers')

nline.make {
  vim.mode(),
  vim.space(),
  icons.git, vim.space(), git.branch(),
  vim.space(), vim.seperator(),
  icons.file,
  vim.space(),
  vim.filename(),
  vim.modified(),
  vim.seperator(),
  wrappers.square_brackets(lsp.current_function()),
  wrappers.square_brackets(git.changes()),
  vim.space(),
  wrappers.square_brackets(lsp.diagnostics()),
  vim.space(),
  lsp.progress(),
  wrappers.square_brackets(vim.line()..vim.space()..vim.colon()..vim.col()),
  vim.filetype(),
}
