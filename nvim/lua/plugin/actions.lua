local has_actions, actions = pcall(require, "actions")
if not has_actions then
  return
end

local floating = require "floating"
local lsp = require "plugin.lsp"
local utils = require "actions.utils"

local function floating_window_opts(opts)
  local base = {
    height_pct = 50,
    width_pct = 70,
  }
  if opts then
    for k, v in pairs(opts) do
      base[k] = v
    end
  end
  return base
end

actions:setup {
  mappings = {
    ["n ,ab"] = "build",
    ["n ,at"] = "test_all",
    ["n ,tt"] = "test_this",
    ["n ,ar"] = "run",
    ["n ,af"] = "format",
  },
  {
    predicate = utils.make_language_predicate "lua",
    actions = {
      run = function(bufnr)
        vim.c.luafile(vim.api.nvim_buf_get_name(bufnr))
      end,
      format = function(bufnr)
        require("stylua"):run(bufnr)
      end,
    },
  },
  {
    predicate = utils.make_path_predicate "gitlab.snapp.ir",
    actions = {
      -- format = function() end,
    },
  },
  {
    predicate = utils.make_language_predicate "go",
    actions = {
      format = function(bufnr)
        local timeoutms = 1000
        local context = { source = { organizeImports = true } }
        vim.validate { context = { context, "t", true } }
        local params = vim.lsp.util.make_range_params()
        params.context = context
        local method = "textDocument/codeAction"
        local resp = vim.lsp.buf_request_sync(0, method, params, timeoutms)
        if resp and resp[1] then
          local result = resp[1].result
          if result and result[1] then
            local edit = result[1].edit
            vim.lsp.util.apply_workspace_edit(edit)
          end
        end
        vim.lsp.buf.formatting_sync()
      end,
      build = function(_)
        floating:command(
          "go build",
          floating_window_opts {
            cwd = lsp.go_root(),
          }
        )
      end,
      test_all = function(_)
        floating:command(
          "go test -v ./...",
          floating_window_opts {
            jobstart = {
              cwd = lsp.go_root(),
            },
          }
        )
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
        floating:command(
          ("go test -v -run %s"):format(current_test),
          floating_window_opts {
            jobstart = {
              cwd = lsp.go_root(),
            },
          }
        )
      end,
    },
  },
}

vim.autocmd {
  "BufWritePre",
  "*",
  function()
    Actions:exec(0, "format")
  end,
}
