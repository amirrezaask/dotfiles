M = {}
local uv = vim.loop

local function spawn(cmd, args, stdin_text, buf)
  local stdin = uv.new_pipe()
  local stdout = uv.new_pipe()
  local stderr = uv.new_pipe()

  print("stdin", stdin)
  print("stdout", stdout)
  print("stderr", stderr)

  local handle, pid = uv.spawn(cmd, {
    stdio = { stdin, stdout, stderr },
    args = args,
  }, function(code, signal) -- on exit
    print("exit code", code)
    print("exit signal", signal)
  end)

  print("process opened", handle, pid)

  uv.read_start(stdout, function(err, data)
    assert(not err, err)
    if data then
        vim.api.nvim_buf_set_lines
      print("stdout chunk", stdout, data)
    else
      print("stdout end", stdout)
    end
  end)

  uv.read_start(stderr, function(err, data)
    assert(not err, err)
    if data then
      print("stderr chunk", stderr, data)
    else
      print("stderr end", stderr)
    end
  end)

  if stdin_text then
    uv.write(stdin, stdin_text)
  end

  uv.shutdown(stdin, function()
    print("stdin shutdown", stdin)
    uv.close(handle, function()
      print("process closed", handle, pid)
    end)
  end)
end

function execute()
  local selected = vim.fn.eval 'getline("\'<")[getpos("\'<")[2]-1:getpos("\'>")[2]-1]'
  if #selected == 0 then
    selected = vim.fn.expand "<cword>"
  end
  local stdin_text
  if #vim.split(selected, " ") > 1 then
    local splitted = vim.split(selected, " ")
    table.remove(splitted, 1)
    stdin_text = table.concat(splitted, " ")
  end
  local buf = vim.api.nvim_create_buf(false, true)
  vim.cmd [[ new ]]
  local win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(win, buf)
  spawn("cat", { "/home/amirreza/receipt.html" }, stdin_text, buf)
end
execute()
return M
