-- Run scripts based on file type
--
vim.cmd [[ command! Run lua require'runner'() ]]

local function split(s, sep)
  local fields = {}

  local sep = sep or " "
  local pattern = string.format("([^%s]+)", sep)
  string.gsub(s, pattern, function(c) fields[#fields + 1] = c end)

  return fields
end

local runner_config = {
  vim = function(file)
    vim.cmd(string.format([[ source %s ]], file))
  end,
  lua = function(file)
    vim.cmd(string.format('luafile %s', file))
  end
}

local run_this_file = function()
  local filename = vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf)
  local ext = split(filename, '.')[2]
  local callback = runner_config[ext]
  if callback == nil then
    print('No callback found for filetype :%s', ext) 
  end
  callback(filename)
end

return run_this_file
