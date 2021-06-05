local has_cursorline, cursorline = pcall(require,'nvim-cursorline')
if not has_cursorline then
  return
end

cursorline.setup {
  highlight_styles = {
    -- 'italic',
    -- 'bold',
    'underline'
  }
}
