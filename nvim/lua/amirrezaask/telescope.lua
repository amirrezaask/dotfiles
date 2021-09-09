local has_telescope, _ = pcall(require, "telescope")
local actions = require "telescope.actions"
local action_state = require "telescope.actions.state"
local finders = require "telescope.finders"
local pickers = require "telescope.pickers"
local conf = require("telescope.config").values
local repos = require "amirrezaask.repos"
local telescope = require "telescope"
local wallpaper = require "amirrezaask.wallpaper"
local ivy = require("telescope.themes").get_ivy
local dropdown = require("telescope.themes").get_dropdown
local notheme = function(opts)
  return opts
end
local current_theme = notheme

local function wrap(fn, opts)
  return function()
    fn(current_theme(opts))
  end
end

telescope.setup {
  defaults = {
    prompt_prefix = "> ",
    selection_caret = "> ",
    layout_strategy = "flex",
    sorting_strategy = "ascending",
    borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
    layout_config = {
      width = 0.95,
      height = 0.85,
      -- preview_cutoff = 120,
      prompt_position = "top",

      horizontal = {
        -- width_padding = 0.1,
        -- height_padding = 0.1,
        preview_width = function(_, cols, _)
          if cols > 200 then
            return math.floor(cols * 0.4)
          else
            return math.floor(cols * 0.6)
          end
        end,
      },
    },

    file_ignore_patterns = { "node_modules/.*", ".git/.*", "_site/.*" },
    file_previewer = require("telescope.previewers").vim_buffer_cat.new,
    grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
    qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
    mappings = {
      n = {
        ["<ESC>"] = actions.close,
        ["<C-c>"] = actions.close,
        ["jk"] = actions.close,
        ["kj"] = actions.close,
      },
      i = {
        ["<C-c>"] = actions.close,
        -- ['<ESC>'] = actions.close,
        ["<C-q>"] = actions.send_to_qflist,
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
      },
    },
  },
}

local M = {}

-- telescope.load_extension('fzy_native')
telescope.load_extension "dap"
-- telescope.load_extension "fzf"
telescope.load_extension "fzy_native"

function M.docs()
  local docs = {}
  for group, group_keys in pairs(__MAPS_DOCS) do
    for k, doc in pairs(group_keys) do
      table.insert(docs, string.format("%s [%s] -> %s", group, k, doc))
    end
  end
  for cmd, doc in pairs(__CMDS_DOCS) do
    table.insert(docs, string.format("%s -> %s", cmd, doc))
  end
  pickers.new(current_theme(), {
    prompt_title = "Docs :)",
    finder = finders.new_table {
      results = docs,
    },
    sorter = conf.generic_sorter(),
    previewer = false,
  }):find()
end

function M.grep_string()
  require("telescope.builtin").grep_string {
    shorten_path = true,
    search = vim.fn.input "Grep String> ",
  }
end

function M.base16_theme_selector()
  local theme_names = require("base16.themes"):names()
  pickers.new(current_theme(), {
    finder = finders.new_table {
      results = theme_names,
    },
    sorter = conf.generic_sorter(),
    attach_mappings = function(prompt_bufnr, map)
      local apply = function()
        local theme = action_state.get_selected_entry(prompt_bufnr)[1]
        for k, v in pairs(require "base16.themes") do
          if k == theme then
            v:apply()
          end
        end
      end
      map("i", "<CR>", apply)
      map("n", "<CR>", apply)
      return true
    end,
  }):find()
end

function M.find_src()
  require("telescope.builtin").find_files(current_theme {
    cwd = "~/src",
  })
end

function M.buffer_git_files()
  require("telescope.builtin").git_files(current_theme {
    cwd = vim.fn.expand "%:p:h",
  })
end

function M.projects(path)
  pickers.new(dropdown(), {
    previewer = false,
    finder = finders.new_table {
      results = repos.list_projects { path or "~/src/repos/Personal" },
    },
    sorter = conf.generic_sorter(),
    attach_mappings = function(prompt_bufnr, map)
      local cd_into = function()
        local dir = action_state.get_selected_entry(prompt_bufnr)[1]
        vim.c.cd(dir)
        actions.close(prompt_bufnr)
      end
      map("i", "<CR>", cd_into)
      map("n", "<CR>", cd_into)
      return true
    end,
  }):find()
end

function M.snapp()
  M.projects "~/src/repos/Snapp"
end

function M.installed_plugins()
  require("telescope.builtin").find_files(current_theme {
    cwd = vim.fn.stdpath "data" .. "/site/pack/packer/start/",
    follow = true,
  })
end

function M.edit_configs()
  require("telescope.builtin").find_files(current_theme {
    prompt_title = "> Edit Configs <",
    cwd = "~/src/repos/Personal/dotfiles",
  })
end

function M.edit_neovim()
  require("telescope.builtin").find_files(current_theme {
    layout_strategy = "vertical",
    prompt_title = "> Edit Neovim Config <",
    cwd = "~/.config/nvim",
    previewer = false,
  })
end

function M.edit_awesome()
  require("telescope.builtin").find_files(current_theme {
    layout_strategy = "vertical",
    prompt_title = "> Edit Awesome Config <",
    cwd = "~/.config/awesome",
    previewer = false,
  })
end

function M.edit_zsh()
  require("telescope.builtin").find_files(current_theme {
    prompt_title = "> Edit ZSH Config <",
    cwd = "~/src/github.com/amirrezaask/dotfiles/zsh",
    hidden = true,
  })
end

function M.lsp_workspace_symbols()
  require("telescope.builtin").lsp_workspace_symbols {
    query = vim.fn.input "Query LSP Workspace Symbols: ",
  }
end

function M.git_files()
  require("telescope.builtin").git_files(current_theme())
end

function M.buffer_grep()
  require("telescope.builtin").current_buffer_fuzzy_find(dropdown {
    previewer = false,
  })
end

function M.actions(bufnr)
  if not has_telescope then
    vim.api.nvim_err_writeln "Install telescope to use this function"
    return
  end
  if not bufnr then
    bufnr = vim.api.nvim_get_current_buf()
  end
  local telescope_actions = require "telescope.actions"
  local telescope_action_state = require "telescope.actions.state"
  local current_actions = Actions:browser(bufnr)
  local dropdown = require("telescope.themes").get_dropdown
  pickers.new(dropdown(), {
    prompt_title = "> Actions Browser <",
    finder = finders.new_table {
      results = current_actions,
      entry_maker = function(ob)
        return {
          value = ob[2],
          display = ob[1],
          ordinal = ob[1],
        }
      end,
    },
    sorter = conf.generic_sorter(),
    attach_mappings = function(prompt_bufnr, map)
      local run = function()
        local fn = telescope_action_state.get_selected_entry(prompt_bufnr).value
        telescope_actions.close(prompt_bufnr)
        fn(bufnr)
      end
      map("i", "<CR>", run)
      map("n", "<CR>", run)
      return true
    end,
  }):find()
end

function M.commands(pat)
  if not has_telescope then
    vim.api.nvim_err_writeln "Install telescope to use this function"
    return
  end
  pickers.new(dropdown(), {
    prompt_title = "> Command Browser <",
    finder = finders.new_table {
      results = vim.fn.getcompletion(pat or "", "command"),
      entry_maker = function(command)
        return {
          value = command,
          display = command,
          ordinal = command,
        }
      end,
    },
    sorter = conf.generic_sorter(),
    attach_mappings = function(prompt_bufnr, map)
      local run = function()
        local command = action_state.get_selected_entry(prompt_bufnr).value
        actions.close(prompt_bufnr)
        vim.cmd(command)
      end
      map("i", "<CR>", run)
      map("n", "<CR>", run)
      return true
    end,
  }):find()
end
vim.cmd [[command! -nargs=* Command lua require('plugin.telescope').commands(<f-args>) ]]

function M.quickfix()
  if _G.quickfix_state == "open" then
    vim.c.cclose()
  else
    require("telescope.builtin").quickfix()
  end
end

-- Took from TJ config
function M.grep_last_search(opts)
  opts = opts or {}

  -- \<getreg\>\C
  -- -> Subs out the search things
  local register = vim.fn.getreg("/"):gsub("\\<", ""):gsub("\\>", ""):gsub("\\C", "")

  opts.shorten_path = true
  opts.word_match = "-w"
  opts.search = register

  print("$ " .. register)
  require("telescope.builtin").grep_string(opts)
end

function M.telescope_commands()
  local output = {}
  for name, fn in pairs(require "telescope.builtin") do
    table.insert(output, { name, fn })
  end
  for name, fn in pairs(require "amirrezaask.telescope") do
    table.insert(output, { name, fn })
  end
  if not has_telescope then
    vim.api.nvim_err_writeln "Install telescope to use this function"
    return
  end
  pickers.new(dropdown(), {
    prompt_title = "> Telescope Browser <",
    finder = finders.new_table {
      results = output,
      entry_maker = function(entry)
        return {
          value = entry[2],
          display = entry[1],
          ordinal = entry[1],
        }
      end,
    },
    sorter = conf.generic_sorter(),
    attach_mappings = function(prompt_bufnr, map)
      local run = function()
        local command = action_state.get_selected_entry(prompt_bufnr).value
        actions.close(prompt_bufnr)
        vim.cmd [[ startinsert ]]
        command()
      end
      map("i", "<CR>", run)
      map("n", "<CR>", run)
      return true
    end,
  }):find()
end

function M.on_attach(_)
  vim.nmap {
    ["gd"] = { wrap(require("telescope.builtin").lsp_definitions), "Goto defenition", "IDE" },
    ["gi"] = { wrap(require("telescope.builtin").lsp_implementations), "Goto implementations", "IDE" },
    ["gr"] = { wrap(require("telescope.builtin").lsp_references), "Goto references", "IDE" },
    ["?d"] = { wrap(require("telescope.builtin").lsp_document_symbols), "Search through document symbols", "IDE" },
    ["?w"] = { wrap(require("amirrezaask.telescope").lsp_workspace_symbols), "Search through workspace symbols", "IDE" },
    ["?c"] = { wrap(require("telescope.builtin").lsp_code_actions), "Show code actions", "IDE" },
    ["'d"] = { wrap(require("telescope.builtin").lsp_document_diagnostics), "Search through document diagnostic", "IDE" },
    ["'w"] = {
      wrap(require("telescope.builtin").lsp_workspace_diagnostics),
      "Search through workspace diagnostics",
      "IDE",
    },
    ["'c"] = { wrap(require("telescope.builtin").lsp_code_actions), "code actions", "IDE" },
  }
end

vim.nmap {
  ["<leader><leader>"] = wrap(require("telescope.builtin").find_files, { hidden = true }),
  ["<leader>fb"] = wrap(require("telescope.builtin").file_browser),
  ["<leader>fp"] = M.installed_plugins,
  ["<leader>pp"] = M.projects,
  ["<leader>ps"] = M.snapp,
  ["<C-p>"] = M.git_files,
  ["<C-q>"] = M.quickfix,
  ["<M-q>"] = require("telescope.builtin").quickfix,
  ["\\\\"] = wrap(M.buffer_grep),
  ["\\l"] = wrap(M.grep_last_search),
  ["??"] = wrap(require("telescope.builtin").live_grep),
  [",f"] = wrap(M.grep_string),
  [",s"] = wrap(require("telescope.builtin").grep_string),
  ["<leader>b"] = wrap(require("telescope.builtin").buffers),
  [",c"] = M.edit_configs,
  ["<leader>ec"] = M.edit_configs,
  ["<leader>L"] = M.telescope_commands,
  ["<leader>en"] = M.edit_neovim,
  [",n"] = M.edit_neovim,
  [",z"] = M.edit_zsh,
  [",a"] = M.edit_awesome,
  ["<leader>fs"] = M.find_src,
  [",w"] = wallpaper.set_wallpaper,
  ["<leader>c"] = wrap(require("telescope.builtin").commands),
  ["<leader>fr"] = wrap(require("telescope.builtin").oldfiles),
  ["<leader>h"] = wrap(require("telescope.builtin").help_tags),
  -- Git
  ["<leader>gc"] = wrap(require("telescope.builtin").git_commits),
  ["<leader>gb"] = wrap(require("telescope.builtin").git_bcommits),
  ["<leader>go"] = wrap(require("telescope.builtin").git_checkout),
  ["<leader>gf"] = M.buffer_git_files,
  ["<leader>gs"] = wrap(require("telescope.builtin").git_status),
  ["<leader>tf"] = require("telescope.builtin").treesitter,
}

return setmetatable(M, {
  __index = function(tbl, k)
    R "amirrezaask.telescope"
    if tbl[k] then
      return tbl[k]
    else
      return require("telescope.builtin")[k]
    end
  end,
})
