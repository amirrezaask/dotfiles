if not require("core.utils").has_plugins { "mason", "mason-lspconfig", "lspconfig" } then
  return
end

Mason = {}
Mason.install_path = require("mason-core.path").concat { vim.fn.stdpath "data", "mason" }

require("mason").setup {
  install_root_dir = Mason.install_path,
}

require("mason-lspconfig").setup {
  automatic_installation = true,
}

function Mason.has(name)
  return vim.fn.filereadable(require("mason-core.path").concat { Mason.install_path, "bin", name }) == 1
end

function Mason.install(to_install)
  local missing = {}

  for _, name in pairs(to_install) do
    if not Mason.has(name) then
      table.insert(missing, name)
    end
  end
  require("mason.api.command").MasonInstall(missing)
end
