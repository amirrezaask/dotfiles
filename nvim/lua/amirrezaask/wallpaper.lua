local M = {}
local path = os.getenv "WALLPAPERS_PATH" or "~/src/github.com/amirrezaask/dotfiles/wallpapers/"

function M.set_wallpaper()
  require("telescope.builtin").find_files {
    cwd = path,
    prompt_title = "Set Wallpaper",
    previewer = false,
    attach_mappings = function(prompt_bufnr, map)
      local apply = function()
        local selected = require("telescope.actions.state").get_selected_entry(prompt_bufnr)
        vim.fn.system(string.format("feh --bg-fill %s", selected.cwd .. selected.value))
      end
      map("i", "<CR>", apply)
      map("n", "<CR>", apply)
      return true
    end,
  }
end

return M
