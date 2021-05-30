MNIM_BUFFERS = {}
local filter = function(b)
  return vim.fn.buflisted(b) == 1 and vim.api.nvim_buf_get_option(b, 'filetype') ~= ''
end

vim.autocmd {
  "BufEnter",
  "*",
  function()
    local bufnr = vim.api.nvim_get_current_buf()
    if filter(bufnr) then
      if not vim.tbl_contains(MNIM_BUFFERS, bufnr) then
        table.insert(MNIM_BUFFERS, bufnr)
      end
    end
  end
}
vim.map {
  ['n `'] = function()
    vim.api.nvim_set_current_buf(MNIM_BUFFERS[#MNIM_BUFFERS-1])
    local tmp = MNIM_BUFFERS[#MNIM_BUFFERS-1]
    MNIM_BUFFERS[#MNIM_BUFFERS-1] = MNIM_BUFFERS[#MNIM_BUFFERS]
    MNIM_BUFFERS[#MNIM_BUFFERS] = tmp
  end,
  ['<M-`>'] = function()
    local pickers = require('telescope.pickers')
    local finders = require('telescope.finders')
    local conf = require('telescope.config').values
    local actions = require('telescope.actions')
    local action_state = require('telescope.actions.state')
    local buffers = {}
    local buffers_rev = {}
    for _, b in ipairs(MNIM_BUFFERS) do
      table.insert(buffers, string.format('%s: %s', b, vim.api.nvim_buf_get_name(b)))
    end
    for i=#buffers, 1, -1 do
      table.insert(buffers_rev, buffers[i])
    end
    pickers.new({}, {
    prompt_title = "> MNIM BUFFER SWITCH <",
    -- TODO: add previewer
    previewer = false,
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
        for idx, b in ipairs(MNIM_BUFFERS) do
          if b == tonumber(bufnr) then
            n = idx
          end
        end
        table.remove(MNIM_BUFFERS, n)
        table.insert(MNIM_BUFFERS, tonumber(bufnr))
        vim.api.nvim_set_current_buf(bufnr)
      end
      map('i', '<CR>', jump_to)
      map('n', '<CR>', jump_to)
      return true
    end,
  }):find()

  end
}

