CtrlTabBuffers = {}
CtrlTabConfig = {}
local ctrltab = {}

function ctrltab.setup(opts)
  opts = opts or {}
  opts.max_buffer_count = opts.max_buffer_count or 10
  CtrlTabConfig = opts
  vim.cmd [[augroup CtrlTab
    autocmd!
    autocmd BufEnter * lua require('ctrltab').add_to_buffer_list()
    augroup END
  ]]
end

function ctrltab:jump_to_last()
  vim.api.nvim_set_current_buf(CtrlTabBuffers[#CtrlTabBuffers - 1])
  local tmp = CtrlTabBuffers[#CtrlTabBuffers - 1]
  CtrlTabBuffers[#CtrlTabBuffers - 1] = CtrlTabBuffers[#CtrlTabBuffers]
  CtrlTabBuffers[#CtrlTabBuffers] = tmp
end

function ctrltab:telescope_switcher()
  local has_telescope, _ = require "telescope"
  if not has_telescope then
    vim.api.nvim_err_writeln "Install telescope to use this function"
    return
  end
  local pickers = require "telescope.pickers"
  local finders = require "telescope.finders"
  local conf = require("telescope.config").values
  local actions = require "telescope.actions"
  local action_state = require "telescope.actions.state"
  local buffers = {}
  local buffers_rev = {}
  for _, b in ipairs(CtrlTabBuffers) do
    table.insert(buffers, string.format("%s: %s", b, vim.api.nvim_buf_get_name(b)))
  end
  for i = #buffers, 1, -1 do
    table.insert(buffers_rev, buffers[i])
  end
  local opts = {}
  pickers.new(opts, {
    prompt_title = "> CtrlTab Switcher <",
    finder = finders.new_table {
      results = buffers_rev,
    },
    sorter = conf.generic_sorter(),
    attach_mappings = function(prompt_bufnr, map)
      local jump_to = function()
        local line = action_state.get_selected_entry(prompt_bufnr)[1]
        local bufnr = vim.split(line, ": ")[1]
        actions.close(prompt_bufnr)
        local n
        for idx, b in ipairs(CtrlTabBuffers) do
          if b == tonumber(bufnr) then
            n = idx
          end
        end
        table.remove(CtrlTabBuffers, n)
        table.insert(CtrlTabBuffers, tonumber(bufnr))
        vim.api.nvim_set_current_buf(bufnr)
      end
      map("i", "<CR>", jump_to)
      map("n", "<CR>", jump_to)
      return true
    end,
  }):find()
end

function ctrltab.add_to_buffer_list()
  local filter = function(b)
    return vim.fn.buflisted(b) == 1 and vim.api.nvim_buf_get_option(b, "filetype") ~= ""
  end
  local index_of = function(t, e)
    for idx, elm in ipairs(t) do
      if elm == e then
        return idx
      end
    end
  end
  local add_to_list = function(b)
    if vim.tbl_contains(CtrlTabBuffers, b) then
      local idx = index_of(CtrlTabBuffers, b)
      table.remove(CtrlTabBuffers, idx)
    end
    if #CtrlTabBuffers > (CtrlTabConfig.max_buffer_count or 10) then
      table.remove(CtrlTabBuffers, 1)
    end
    table.insert(CtrlTabBuffers, b)
  end
  local bufnr = vim.api.nvim_get_current_buf()
  if filter(bufnr) then
    add_to_list(bufnr)
  end
end

return ctrltab
