return function(pattern, callback)
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
