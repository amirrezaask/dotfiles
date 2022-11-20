function onsave(pattern, callback)
  local augroup_name = ""
  if type(pattern) == "table" then
    augroup_name = table.concat(pattern, ",") .. "-onsave"
  end
  if type(pattern) == "string" then
    augroup_name = pattern .. "-onsave"
  end
  vim.api.nvim_create_autocmd("BufWritePost", {
    group = vim.api.nvim_create_augroup(augroup_name, {}),
    pattern = pattern,
    callback = callback,
  })
end
function bind(spec)
  for mode, keys in pairs(spec) do
    for key, binding in pairs(keys) do
      if type(binding) == "string" or type(binding) == "function" then
        vim.keymap.set(mode, key, binding)
      else
        if type(binding) == "table" then
          -- { function or string, doc }
          local handler = binding[1]
          table.remove(binding, 1)
          vim.keymap.set(mode, key, handler, binding)
        end
      end
    end
  end
end
window_height = function()
  return vim.api.nvim_win_get_height(0)
end

window_width = function()
  return vim.api.nvim_win_get_width(0)
end

