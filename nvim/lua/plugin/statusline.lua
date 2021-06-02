local has_nline, nline = pcall(require, 'nline')
if not has_nline then
  return
end

local parts = nline.parts
local git = require('nline.parts.git')
local lsp = require('nline.parts.lsp')
local wrappers = nline.wrappers

nline.make {
  parts.mode(),
  parts.space,
  parts.icons.git, parts.space, git.branch,
  parts.space, parts.seperator,
  parts.icons.file,
  parts.space,
  parts.filename,
  parts.modified,
  parts.seperator,
  wrappers.square_brackets(lsp.current_function()),
  wrappers.square_brackets(git.changes()),
  parts.space,
  wrappers.square_brackets(lsp.diagnostics()),
  parts.space,
  lsp.progress,
  wrappers.square_brackets(parts.line..parts.space..parts.colon..parts.col),
  parts.filetype,
}
