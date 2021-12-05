local actions = require "actions"
local utils = require "actions.utils"

actions:setup {
  mappings = {
    ["n ,ab"] = "build",
    ["n ,at"] = "test_all",
    ["n ,tt"] = "test_this",
    ["n ,ar"] = "run",
    ["n ,af"] = "format",
  },
  {
    predicate = utils.make_language_predicate "vim",
    actions = {
      run = function(_)
        vim.cmd [[ so % ]]
      end,
    },
  },
  {
    predicate = utils.compose(utils.make_language_predicate "lua", utils.make_path_predicate "plugins.lua"),
    actions = {
      run = function(bufnr)
        vim.cmd [[ luafile % ]]
        vim.cmd [[ PackerInstall ]]
      end,
    },
  },
  {
    predicate = utils.make_language_predicate "lua",
    actions = {
      run = function(_)
        vim.cmd [[ so % ]]
      end,
      format = function(bufnr)
        require("stylua"):run(bufnr)
      end,
    },
  },
  {
    predicate = utils.make_language_predicate "rust",
    actions = {
      format = function()
        vim.lsp.buf.formatting_sync()
      end,
      run = function(_)
        vim.cmd [[ vnew | term cargo run ]]
      end,
      build = function(_)
        vim.cmd [[ vnew | term cargo check ]]
      end,
      test_all = function(_)
        vim.cmd [[ RustTest! ]]
      end,
      test_this = function(_)
        vim.cmd [[ RustTest ]]
      end,
    },
  },
  {
    predicate = utils.make_path_predicate "gitlab.snapp.ir",
    actions = {
      format = function() end,
    },
  },
  {
    predicate = utils.make_language_predicate "go",
    actions = {
      format = function(_)
        vim.lsp.buf.formatting()
      end,
      build = function(_)
        vim.cmd [[ vnew | term go build ]]
      end,
      run = function()
        vim.cmd [[ vnew | term go run *.go ]]
      end,
      test_all = function(_)
        vim.cmd [[ vnew | term go test -v ./... ]]
      end,
      test_this = function(_)
        local function go_current_test()
          local linenr = vim.fn.search("func \\(Test\\|Example\\)", "bcnW")
          if linenr == 0 then
            return
          end
          local linetext = vim.fn.getline(linenr)
          local test_name = vim.split(linetext, " ")[2]
          local start, _ = string.find(test_name, "%(")
          test_name = string.sub(test_name, 1, start - 1)
          return test_name
        end
        local current_test = go_current_test()
        vim.cmd(string.format([[ vnew | term go test -v -run %s ]], current_test))
      end,
    },
  },
}
-- Auto formatting for lua
vim.cmd [[ autocmd BufWritePre *.lua lua Actions:exec(0, 'format') ]]
vim.cmd [[ autocmd BufWritePre *.rs lua Actions:exec(0, 'format') ]]
