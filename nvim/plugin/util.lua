function Missing(mod)
  local ok, _ = pcall(require, mod)
  return not ok
end

function Has(mod)
  local ok, _ = pcall(require, mod)
  return ok
end
