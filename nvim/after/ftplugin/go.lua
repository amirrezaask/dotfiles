vim.bo[0].sw = 4
vim.bo[0].ts = 4
vim.bo[0].expandtab = false
vim.bo[0].shiftwidth = 4

local function run_go_command_in_split(command_with_opts)
  return function()
    print("running " .. table.concat(command_with_opts, " "))
    local cwd = vim.fn.getcwd()

    vim.system(command_with_opts, { cwd = cwd }, function(obj)
      vim.schedule(function()
        local msg = ""
        if obj.code ~= 0 then
          msg = "Go Build Failed!"
          vim.notify(msg, vim.log.levels.ERROR)
        else
          msg = "Go Build Succeded!"
          vim.notify(msg, vim.log.levels.INFO)
        end

        if not vim.g.go_build_buffer or not vim.api.nvim_buf_is_valid(vim.g.go_build_buffer) then
          vim.g.go_build_buffer = vim.api.nvim_create_buf(false, true)
        end

        vim.api.nvim_set_option_value("modifiable", true, { buf = vim.g.go_build_buffer })
        local lines = vim.split(obj.stdout .. obj.stderr, "\n", { trimempty = true })
        table.insert(lines, 1, "Go Build Output:")
        table.insert(lines, #lines + 1, msg)

        vim.api.nvim_buf_set_lines(vim.g.go_build_buffer, 0, -1, false, lines)

        vim.api.nvim_set_option_value("buftype", "nofile", { buf = vim.g.go_build_buffer })
        vim.api.nvim_set_option_value("bufhidden", "wipe", { buf = vim.g.go_build_buffer })
        vim.api.nvim_set_option_value("swapfile", false, { buf = vim.g.go_build_buffer })
        vim.api.nvim_set_option_value("modifiable", false, { buf = vim.g.go_build_buffer })

        for _, win in ipairs(vim.api.nvim_list_wins()) do -- Toggle if a window showing terminal is open
          local win_buf = vim.api.nvim_win_get_buf(win)
          if win_buf == vim.g.go_build_buffer then
            return
          end
        end

        vim.api.nvim_open_win(vim.g.go_build_buffer, true, {
          split = "right",
          width = math.floor(vim.o.columns * 0.3),
        })
      end)
    end)
  end
end

vim.keymap.set("n", "<C-enter>", run_go_command_in_split({ "go", "build", "-v", "./..." }), { buffer = 0 })

vim.keymap.set("n", "<M-enter>", run_go_command_in_split({ "go", "test", "-v", "./..." }), { buffer = 0 })
