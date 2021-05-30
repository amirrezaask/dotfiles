local has_nline, nline = pcall(require, 'nline')
if not has_nline then
  return
end
local parts = nline.parts

nline.make {
  parts.mode,
  parts.space,
  parts.icons.git, parts.space, parts.git_branch,
  parts.seperator, parts.space,
  parts.icons.file,
  parts.space,
  parts.filename,
  parts.modified,
  parts.seperator,
  parts.space,
  parts.line_col,
  parts.filetype,
  parts.lsp_status,
}
