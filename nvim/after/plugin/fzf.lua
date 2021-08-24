local emacs_opts = function()
  vim.g.fzf_layout = { down = "50%" }
end
local popup_opts = function()
  vim.g.fzf_layout = { window = { width = 0.9, height = 0.8 } }
end

local default_opts = popup_opts

function FZF(spec)
  default_opts()
  vim.fn.call("fzf#run", { spec })
end

local function fzf_command(cmd, ...)
  default_opts()
  local args = { ... }
  if args then
    args = table.concat(args, " ")
  end
  return function()
    vim.cmd(string.format(":%s %s", cmd, args))
  end
end

if vim.g.fuzzy_finder == "fzf" then
  vim.nmap {
    ["<leader><leader>"] = function()
      vim.cmd [[ Files ]]
    end,
    ["<leader>fp"] = function()
      local plugins = vim.fn.stdpath "data" .. "/site/pack/packer/start/"
      vim.cmd(string.format([[ Files %s]], plugins))
    end,
    ["<leader>ps"] = fzf_command("Files", string.format("%s/src/gitlab.snapp.ir", os.getenv "GOPATH")),
    ["<C-p>"] = fzf_command "GFiles",
    ["\\\\"] = fzf_command "BGrep",
    ["??"] = fzf_command "Rg",
    [",f"] = function()
      fzf_command("Rg", vim.fn.input "Rg> ")()
    end,
    ["<leader>b"] = fzf_command "Buffers",
    [",n"] = fzf_command("Files", "~/.config/nvim"),
    [",z"] = fzf_command("Files", "~/.config/zsh"),
    [",a"] = fzf_command("Files", "~/.config/awesome"),
    ["<leader>fs"] = fzf_command("Files", string.format("%s/src", os.getenv "GOPATH")),
    ["<leader>c"] = fzf_command "Commands",
    ["<leader>fr"] = fzf_command "History",
    ["<leader>h"] = fzf_command "Helptags",
    -- Git
    ["<leader>gc"] = fzf_command "Commits",
    ["<leader>gb"] = fzf_command "BCommits",
    ["<leader>gf"] = function()
      local cwd = vim.fn.expand "%:p:h"
      fzf_command("Files", cwd)()
    end,
  }
end
