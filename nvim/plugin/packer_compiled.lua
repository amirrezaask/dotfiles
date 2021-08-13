-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/home/amirreza/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/amirreza/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/amirreza/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/amirreza/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/amirreza/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  ["FixCursorHold.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/FixCursorHold.nvim"
  },
  ["Vim-Jinja2-Syntax"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/Vim-Jinja2-Syntax"
  },
  ["actions.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/actions.nvim"
  },
  ["ansible-vim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/ansible-vim"
  },
  ["dial.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/dial.nvim"
  },
  ["dockerfile.vim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/dockerfile.vim"
  },
  ["git-worktree.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/git-worktree.nvim"
  },
  ["gitsigns.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/gitsigns.nvim"
  },
  ["haskell-vim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/haskell-vim"
  },
  ["lir.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/lir.nvim"
  },
  ["lsp-status.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/lsp-status.nvim"
  },
  ["lsp_extensions.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/lsp_extensions.nvim"
  },
  ["lspkind-nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/lspkind-nvim"
  },
  ["luv-vimdocs"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/luv-vimdocs"
  },
  ["nginx.vim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nginx.vim"
  },
  ["nline.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nline.nvim"
  },
  ["nlua.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nlua.nvim"
  },
  ["nvim-colorizer.lua"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-colorizer.lua"
  },
  ["nvim-compe"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-compe"
  },
  ["nvim-dap"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-dap"
  },
  ["nvim-dap-virtual-text"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-dap-virtual-text"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-lspconfig"
  },
  ["nvim-luaref"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-luaref"
  },
  ["nvim-nonicons"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-nonicons"
  },
  ["nvim-tree.lua"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-treesitter-textobjects"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-treesitter-textobjects"
  },
  ["nvim-ts-context-commentstring"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-ts-context-commentstring"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/packer.nvim"
  },
  ["palette.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/palette.nvim"
  },
  playground = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/playground"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/popup.nvim"
  },
  ["rust-tools.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/rust-tools.nvim"
  },
  ["rust.vim"] = {
    loaded = false,
    needs_bufread = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/opt/rust.vim"
  },
  tabular = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/tabular"
  },
  ["telescope-dap.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/telescope-dap.nvim"
  },
  ["telescope-fzf-native.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/telescope-fzf-native.nvim"
  },
  ["telescope-fzy-native.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/telescope-fzy-native.nvim"
  },
  ["telescope-github.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/telescope-github.nvim"
  },
  ["telescope-media-files.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/telescope-media-files.nvim"
  },
  ["telescope.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/telescope.nvim"
  },
  ["todo-comments.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/todo-comments.nvim"
  },
  ["vim-commentary"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/vim-commentary"
  },
  ["vim-elixir"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/vim-elixir"
  },
  ["vim-fish"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/vim-fish"
  },
  ["vim-jdaddy"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/opt/vim-jdaddy"
  },
  ["vim-json"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/vim-json"
  },
  ["vim-mscgen"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/vim-mscgen"
  },
  ["vim-nix"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/vim-nix"
  },
  ["vim-ps1"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/vim-ps1"
  },
  ["vim-scriptease"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/vim-scriptease"
  },
  ["vim-surround"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/vim-surround"
  },
  ["vim-syntax-extra"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/vim-syntax-extra"
  },
  ["vim-terraform"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/vim-terraform"
  },
  ["vim-toml"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/vim-toml"
  },
  ["zig.vim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/zig.vim"
  }
}

time([[Defining packer_plugins]], false)
vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time([[Defining lazy-load filetype autocommands]], true)
vim.cmd [[au FileType rust ++once lua require("packer.load")({'rust.vim'}, { ft = "rust" }, _G.packer_plugins)]]
vim.cmd [[au FileType json ++once lua require("packer.load")({'vim-jdaddy'}, { ft = "json" }, _G.packer_plugins)]]
time([[Defining lazy-load filetype autocommands]], false)
vim.cmd("augroup END")
vim.cmd [[augroup filetypedetect]]
time([[Sourcing ftdetect script at: /home/amirreza/.local/share/nvim/site/pack/packer/opt/rust.vim/ftdetect/rust.vim]], true)
vim.cmd [[source /home/amirreza/.local/share/nvim/site/pack/packer/opt/rust.vim/ftdetect/rust.vim]]
time([[Sourcing ftdetect script at: /home/amirreza/.local/share/nvim/site/pack/packer/opt/rust.vim/ftdetect/rust.vim]], false)
vim.cmd("augroup END")
if should_profile then save_profiles() end

end)

if not no_errors then
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
