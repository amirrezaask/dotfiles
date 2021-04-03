local nvim = vim.api

local base = {
   run = function(file)
      return function()
        vim.cmd(":vnew")
        vim.fn.termopen(string.format('go run %s', file))
      end
   end,
   import = function(file)
      return function()
        nvim.nvim_command(string.format('silent ! goimports -w %s', file))
        nvim.nvim_command('e')
      end
   end,
   test = function(path)
      return function()
        nvim.nvim_command(string.format('terminal go test %s', path))
      end
   end,
   watch_test = function(path)
      return function()
        vim.cmd(":vnew")
        vim.fn.termopen(string.format('watch -n 5 go test %s', path))
      end
   end,
   fmt = function(pkg)
      return function()
        nvim.nvim_command(string.format('silent ! go fmt %s', pkg))
        nvim.nvim_command('e')
      end
   end,
   install = function(pkg)
      return function()
        vim.cmd(":vnew")
        vim.fn.termopen(string.format('silent ! go install %s', pkg))
      end
   end
}


local utils = {
   imports = function()
      base.import("%")()
   end,
   test_all = function()
      base.test("./...")()
   end,
   test_tdd = function()
      base.watch_test("./...")()
   end,
   fmt = function()
      base.fmt(".")()
   end,
   install = function()
      base.install(".")
   end
}

local mod = {
   base = base,
   utils = utils,
}

return mod

