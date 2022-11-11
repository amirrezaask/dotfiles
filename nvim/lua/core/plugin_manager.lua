local _plugins = {}

local M = {}

function M.bootstrap()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end


function M.use(spec)
  table.insert(_plugins, spec)
end

function M.install()
  require('packer').startup(function(use)
    for _, spec in ipairs(_plugins) do
      use(spec)
    end
  end)
end

return M
