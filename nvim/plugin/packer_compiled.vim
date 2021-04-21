" Automatically generated packer.nvim plugin loader code

if !has('nvim-0.5')
  echohl WarningMsg
  echom "Invalid Neovim version for packer.nvim!"
  echohl None
  finish
endif

packadd packer.nvim

try

lua << END
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

time("Luarocks path setup", true)
local package_path_str = "/home/amirreza/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/amirreza/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/amirreza/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/amirreza/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/amirreza/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time("Luarocks path setup", false)
time("try_loadstring definition", true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    print('Error running ' .. component .. ' for ' .. name)
    error(result)
  end
  return result
end

time("try_loadstring definition", false)
time("Defining packer_plugins", true)
_G.packer_plugins = {
  ["dockerfile.vim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/dockerfile.vim"
  },
  ["fuzzy.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/fuzzy.nvim"
  },
  fzf = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/fzf"
  },
  ["fzf.vim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/fzf.vim"
  },
  ["git-messenger.vim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/git-messenger.vim"
  },
  ["gitsigns.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/gitsigns.nvim"
  },
  ["indent-blankline.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/indent-blankline.nvim"
  },
  indentLine = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/indentLine"
  },
  ["lazygit.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/lazygit.nvim"
  },
  ["lsp_extensions.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/lsp_extensions.nvim"
  },
  ["lspsaga.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/lspsaga.nvim"
  },
  ["nvim-base16.lua"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-base16.lua"
  },
  ["nvim-colorizer.lua"] = {
    commands = { "ColorizerAttachToBuffer", "ColorizerDetachFromBuffer", "ColorizerToggle", "ColorizerReloadAllBuffers" },
    loaded = false,
    needs_bufread = false,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/opt/nvim-colorizer.lua"
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
  ["nvim-treesitter"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/packer.nvim"
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
  ["rust.vim"] = {
    loaded = false,
    needs_bufread = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/opt/rust.vim"
  },
  ["snippets.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/snippets.nvim"
  },
  ["spawn.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/spawn.nvim"
  },
  ["telescope-dap.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/telescope-dap.nvim"
  },
  ["telescope-fzy-native.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/telescope-fzy-native.nvim"
  },
  ["telescope.nvim"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/telescope.nvim"
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
    loaded = false,
    needs_bufread = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/opt/vim-fish"
  },
  ["vim-jdaddy"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/opt/vim-jdaddy"
  },
  ["vim-nix"] = {
    loaded = false,
    needs_bufread = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/opt/vim-nix"
  },
  ["vim-surround"] = {
    loaded = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/start/vim-surround"
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
    loaded = false,
    needs_bufread = true,
    path = "/home/amirreza/.local/share/nvim/site/pack/packer/opt/zig.vim"
  }
}

time("Defining packer_plugins", false)

-- Command lazy-loads
time("Defining lazy-load commands", true)
vim.cmd [[command! -nargs=* -range -bang -complete=file ColorizerReloadAllBuffers lua require("packer.load")({'nvim-colorizer.lua'}, { cmd = "ColorizerReloadAllBuffers", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file ColorizerAttachToBuffer lua require("packer.load")({'nvim-colorizer.lua'}, { cmd = "ColorizerAttachToBuffer", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file ColorizerDetachFromBuffer lua require("packer.load")({'nvim-colorizer.lua'}, { cmd = "ColorizerDetachFromBuffer", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file ColorizerToggle lua require("packer.load")({'nvim-colorizer.lua'}, { cmd = "ColorizerToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
time("Defining lazy-load commands", false)

vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time("Defining lazy-load filetype autocommands", true)
vim.cmd [[au FileType zig ++once lua require("packer.load")({'zig.vim'}, { ft = "zig" }, _G.packer_plugins)]]
vim.cmd [[au FileType fish ++once lua require("packer.load")({'vim-fish'}, { ft = "fish" }, _G.packer_plugins)]]
vim.cmd [[au FileType rust ++once lua require("packer.load")({'rust.vim'}, { ft = "rust" }, _G.packer_plugins)]]
vim.cmd [[au FileType json ++once lua require("packer.load")({'vim-jdaddy'}, { ft = "json" }, _G.packer_plugins)]]
vim.cmd [[au FileType nix ++once lua require("packer.load")({'vim-nix'}, { ft = "nix" }, _G.packer_plugins)]]
time("Defining lazy-load filetype autocommands", false)
vim.cmd("augroup END")
vim.cmd [[augroup filetypedetect]]
time("Sourcing ftdetect script at: /home/amirreza/.local/share/nvim/site/pack/packer/opt/zig.vim/ftdetect/zig.vim", true)
vim.cmd [[source /home/amirreza/.local/share/nvim/site/pack/packer/opt/zig.vim/ftdetect/zig.vim]]
time("Sourcing ftdetect script at: /home/amirreza/.local/share/nvim/site/pack/packer/opt/zig.vim/ftdetect/zig.vim", false)
time("Sourcing ftdetect script at: /home/amirreza/.local/share/nvim/site/pack/packer/opt/vim-nix/ftdetect/nix.vim", true)
vim.cmd [[source /home/amirreza/.local/share/nvim/site/pack/packer/opt/vim-nix/ftdetect/nix.vim]]
time("Sourcing ftdetect script at: /home/amirreza/.local/share/nvim/site/pack/packer/opt/vim-nix/ftdetect/nix.vim", false)
time("Sourcing ftdetect script at: /home/amirreza/.local/share/nvim/site/pack/packer/opt/vim-fish/ftdetect/fish.vim", true)
vim.cmd [[source /home/amirreza/.local/share/nvim/site/pack/packer/opt/vim-fish/ftdetect/fish.vim]]
time("Sourcing ftdetect script at: /home/amirreza/.local/share/nvim/site/pack/packer/opt/vim-fish/ftdetect/fish.vim", false)
time("Sourcing ftdetect script at: /home/amirreza/.local/share/nvim/site/pack/packer/opt/rust.vim/ftdetect/rust.vim", true)
vim.cmd [[source /home/amirreza/.local/share/nvim/site/pack/packer/opt/rust.vim/ftdetect/rust.vim]]
time("Sourcing ftdetect script at: /home/amirreza/.local/share/nvim/site/pack/packer/opt/rust.vim/ftdetect/rust.vim", false)
vim.cmd("augroup END")
if should_profile then save_profiles() end

END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry
