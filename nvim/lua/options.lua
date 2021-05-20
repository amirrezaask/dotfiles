vim.opt = setmetatable({}, {
  __index = function(_, k)
    return vim.o[k]
  end,
  __newindex = function(_, k, v)
    if type(v) == "boolean" then
      if v then
        vim.cmd(string.format([[ set %s ]], k))
      else
        vim.cmd(string.format([[ set no%s ]], k))
      end
      return
    elseif type(v) == "table" and vim.tbl_islist(v) then
      vim.cmd(string.format([[ set %s=%s ]], k, table.concat(v, ',')))
    else
      vim.cmd(string.format([[ set %s=%s ]], k, v))
    end
  end
})

vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.equalalways = false
vim.opt.modeline = true
vim.opt.autoread = true
vim.opt.compatible = false
vim.opt.encoding = 'utf-8'
vim.opt.hlsearch = true
vim.opt.history = 700
vim.opt.tabpagemax = 100
vim.opt.ruler = true
vim.opt.mouse = 'a'
vim.opt.wrap = true
vim.opt.autoindent = true
vim.opt.termguicolors = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.cursorline = true
vim.opt.relativenumber = true
vim.opt.number = true
vim.opt.pumblend = 13
vim.opt.showmode = false
vim.opt.clipboard = 'unnamedplus'

