CtrlTabBuffers = {}

local ctrltab = {}

function ctrltab:jump_to_last()
  vim.api.nvim_set_current_buf(CtrlTabBuffers[#CtrlTabBuffers-1])
  local tmp = CtrlTabBuffers[#CtrlTabBuffers-1]
  CtrlTabBuffers[#CtrlTabBuffers-1] = CtrlTabBuffers[#CtrlTabBuffers]
  CtrlTabBuffers[#CtrlTabBuffers] = tmp
end

function ctrltab:telescope_switcher()
  local has_telescope, _ = require('telescope')
  if not has_telescope then
    vim.api.nvim_err_writeln('Install telescope to use this function')
  end
  local pickers = require('telescope.pickers')
  local finders = require('telescope.finders')
  local conf = require('telescope.config').values
  local actions = require('telescope.actions')
  local action_state = require('telescope.actions.state')
  local buffers = {}
  local buffers_rev = {}
  for _, b in ipairs(CtrlTabBuffers) do
    table.insert(buffers, string.format('%s: %s', b, vim.api.nvim_buf_get_name(b)))
  end
  for i=#buffers, 1, -1 do
    table.insert(buffers_rev, buffers[i])
  end
  local opts = {}
  pickers.new(opts, {
    prompt_title = "> CtrlTab Switcher <",
    finder = finders.new_table({
      results = buffers_rev,
    }),
    sorter = conf.generic_sorter(),
    attach_mappings = function(prompt_bufnr, map)
      local jump_to = function()
        local line = action_state.get_selected_entry(prompt_bufnr)[1]
        local bufnr = vim.split(line, ': ')[1]
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
      map('i', '<CR>', jump_to)
      map('n', '<CR>', jump_to)
      return true
    end,
  }):find()
end

function ctrltab.add_to_buffer_list()
  local filter = function(b)
    return vim.fn.buflisted(b) == 1 and vim.api.nvim_buf_get_option(b, 'filetype') ~= ''
  end
  local bufnr = vim.api.nvim_get_current_buf()
  if filter(bufnr) then
    if not vim.tbl_contains(CtrlTabBuffers, bufnr) then
      table.insert(CtrlTabBuffers, bufnr)
    end
  end
end

--TODO: should store only last n items
vim.cmd [[ augroup CtrlTab
  au!
  autocmd BufEnter * lua require('ctrltab').add_to_buffer_list()
  augroup END
]]

return ctrltab
