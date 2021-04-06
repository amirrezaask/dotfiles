-- Reloader
function RELOAD(pkg)
  package.loaded[pkg] = nil
  require(pkg)
end

-- Printer
function P(obj)
  print(vim.inspect(obj))
end

-- Eval line
function EVAL()
 local filetype = vim.api.nvim_buf_get_option(0, 'filetype') 
 local line = vim.fn.getline('.')
 if filetype == 'lua' then
  vim.cmd(string.format([[ lua %s ]], line)) 
 end
end

require'nvim'.mode_map({
  n = { [ '<Space>x' ] = '<cmd>lua EVAL()<CR>',
      ['<Space>X'] = '<cmd>luafile %<CR>',}})


