local has_nline, statusline = pcall(require, 'statusline')
if not has_nline then
  return
end
local parts = statusline.parts
local wrappers = statusline.wrappers

statusline.make {
  parts.mode,
  parts.space,
  parts.icons.git, parts.space, parts.git_branch,
  parts.space, parts.lsp_status,
  parts.seperator, parts.space,
  parts.icons.file,
  parts.space,
  parts.filename_shorten,
  parts.modified,
  parts.seperator,
  parts.space,
  parts.line_col,
  parts.filetype,
}
