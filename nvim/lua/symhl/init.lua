local default_config = {
  hl = 'CursorLine',
  ns = vim.api.nvim_create_namespace('SymbolHighlight'),
}

local symhl = {}
symhl.__index = symhl

function symhl:cursor_moved()
  local current_word = vim.fn.expand('<cword>')
  local start = vim.fn.getline('.'):find(current_word)
  local _end = start+#current_word
  local line = vim.api.nvim_win_get_cursor(0)[1]
  vim.api.nvim_buf_clear_highlight(0,default_config.ns,line-2,line-1)
  vim.api.nvim_buf_add_highlight(0, default_config.ns, self.hl or default_config.hl, line-1, start-1, _end-1)
end


function symhl:setup(opts)
  symhl = setmetatable(opts, self)
  vim.cmd [[augroup SymHL]]
  vim.cmd [[au!]]
  vim.cmd [[au CursorMoved,CursorMovedI * lua require('symhl'):cursor_moved()]]
  vim.cmd [[augroup END]]
end

return symhl
