function conf.fzf()
  if config(plugins, "fuzzy_finder") ~= "fzf" then
    return
  end
  bind {
    n = {
      ["<leader><leader>"] = { ":Files<cr>", desc = "Find Files" },
      ["<leader>ff"] = { "<cmd>Files<cr>", desc = "Find Files" },
      ["<leader>fn"] = { "<cmd>Files ~/.config/nvim<CR>", desc = "Neovim Config" },
      ["<leader>fg"] = { "<cmd>GFiles<cr>", desc = "Git Files" },
      ["<leader>fr"] = { "<cmd>History<cr>", desc = "Recent Files" },
      ["<leader>fh"] = { "<cmd>Helptags<CR>", desc = "Help" },
      ["<leader>p"] = { "<cmd>Commands<cr>", desc = "Command palete" },
      ["<leader>fc"] = { "<cmd>Commands<cr>", desc = "Command palete" },
      ["??"] = { "<cmd>Rg<cr>", desc = "live grep" },
    },
  }
end
if config(plugins, "fuzzy_finder") == "fzf" then
  plugin { "junegunn/fzf", run = ":call fzf#install()" }
  plugin {
    "junegunn/fzf.vim",
    config = conf.fzf,
  }
end
