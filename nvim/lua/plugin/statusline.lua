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
  parts.space, parts.seperator,
  parts.icons.file,
  parts.space,
  parts.filename,
  parts.modified,
  parts.seperator,
  wrappers.square_brackets(parts.lsp_current_function()),
  wrappers.square_brackets(parts.git_changes()),
  parts.space,
  wrappers.square_brackets(parts.lsp_diagnostics()),
  parts.space,
  parts.lsp_progress,
  wrappers.square_brackets('%l:%c'),
  parts.filetype,
}
