local lua = {}

-- Force reload the module
function R(mod)
  package.loaded[mod] = nil
  return require(mod)
end

-- is the package loaded ?
function L(mod)
  return package.loaded[mod] ~= nil
end

-- Printer
function P(obj)
  print(vim.inspect(obj))
end

-- Eval line
function EvalLine()
  vim.cmd [[ w ]]
  local lnum = vim.api.nvim_win_get_cursor(0)[1]
  local line = vim.api.nvim_buf_get_lines(0, lnum - 1, lnum, false)[1]
  local function wrap(l)
    return string.format('return(vim.inspect(%s))', l)
  end
  local result = load(wrap(line))()
  if result ~= nil then
    local inlay = require('amirrezaask.inlayhints').for_buf(vim.api.nvim_get_current_buf())
    inlay:set({
      lnum = lnum - 1,
      line = result
    })
    vim.cmd [[ w ]]
    vim.cmd [[ e ]]
  end
end

function lua.format(opts)
  if vim.fn.executable('stylua') ~= 1 then
    print('Error: stylua not found')
    return
  end
  opts = opts or {}
  opts.config = opts.config or os.getenv('HOME') .. '/.stylua.toml'
  local filename = vim.api.nvim_buf_get_name(0)
  vim.cmd([[ w ]])
  vim.cmd(string.format([[ silent ! stylua --config-path %s %s ]], opts.config, filename))
  vim.cmd([[ e ]])
end

vim.command('LuaFormat', lua.format)

vim.augroup({
  lua = {
    'BufEnter',
    '*.lua',
    'set ts=2 sw=2 sts=2 expandtab',
  },
})
LuaAutoFormat = true

vim.cmd([[ nnoremap ,nf <cmd>lua LuaAutoFormat=false<CR> ]])

vim.api.nvim_buf_set_keymap(0, 'n', ',f', '<cmd>LuaFormat<CR>', { noremap = true })

vim.map {
    ['n ,x'] = '<cmd>lua EvalLine()<CR>',
    ['n <leader>x'] = '<cmd>luafile %<CR>',
}

return lua
