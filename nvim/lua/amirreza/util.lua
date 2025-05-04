function Missing(mod)
  local ok, _ = pcall(require, mod)
  return not ok
end

function Has(mod)
  local ok, _ = pcall(require, mod)
  return ok
end

function Theme()
  vim.ui.select(vim.fn.getcompletion("", "color"), {
    prompt = "Select a theme: ",
  }, function(choice)
    if choice == "" or choice == nil then
      return
    end
    vim.cmd("colorscheme " .. choice)
  end)
end

vim.api.nvim_create_user_command("Theme", Theme, {})
