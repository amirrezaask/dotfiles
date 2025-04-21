math.randomseed(os.time())

local version_string = vim.version().major .. "." .. vim.version().minor .. "." .. vim.version().patch

local ascii_arts = {
    {
        "  ███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗",
        "  ████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║",
        "  ██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║",
        "  ██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║",
        "  ██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║",
        "  ╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝",
        version_string
    },
}

local function write_start_screen_to_buffer_and_window(buf, win)
    vim.bo[buf].bufhidden = "wipe"
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].swapfile = false
    vim.bo[buf].modifiable = true

    local original_list = vim.wo[win].list
    local original_number = vim.wo[win].number
    local original_relativenumber = vim.wo[win].relativenumber

    vim.wo[win].list = false
    vim.wo[win].number = false
    vim.wo[win].relativenumber = false

    vim.api.nvim_create_autocmd("BufLeave", {
        buffer = buf,
        callback = function()
            vim.wo[win].list = original_list
            vim.wo[win].number = original_number
            vim.wo[win].relativenumber = original_relativenumber
        end,
    })

    local width = vim.o.columns
    local height = vim.o.lines

    local selected_art = ascii_arts[math.random(1, #ascii_arts)]

    local max_width = 0
    for _, line in ipairs(selected_art) do
        max_width = math.max(max_width, vim.fn.strdisplaywidth(line))
    end

    local start_row = math.max(0, math.floor((height - #selected_art) / 2))

    local lines = {}
    for _ = 1, start_row do
        table.insert(lines, "")
    end
    for _, line in ipairs(selected_art) do
        local line_width = vim.fn.strdisplaywidth(line)
        local padding = math.max(0, math.floor((width - line_width) / 2))
        table.insert(lines, string.rep(" ", padding) .. line)
    end

    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)

    vim.api.nvim_set_current_buf(buf)

    vim.bo[buf].modifiable = false
    vim.bo[buf].filetype = "startscreen"
    vim.api.nvim_create_autocmd("WinResized", {
        callback = function()
            if height ~= vim.o.lines or width ~= vim.o.columns then
                write_start_screen_to_buffer_and_window(buf, win)
            end
        end
    })
end

vim.api.nvim_create_augroup("StartScreen", { clear = true })

vim.api.nvim_create_autocmd("VimEnter", {
    group = "StartScreen",
    callback = function()
        if vim.fn.argc() == 0 and vim.fn.line2byte(1) == -1 then
            local buf = vim.api.nvim_create_buf(false, true)
            local win = vim.api.nvim_get_current_win()

            write_start_screen_to_buffer_and_window(buf, win)
        end
    end,
})
