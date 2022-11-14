-- Simple Fuzzy finder to replace telescope
local M = {}
local default_sorter = nil
local locations = {
    center = function(win_height, win_width)
        local width = vim.api.nvim_get_option('columns')
        local height = vim.api.nvim_get_option('lines')
        local row = math.ceil((height - win_height) / 2 - 1)
        local col = math.ceil((width - win_width) / 2)
        return row, col
    end,
    bottom_center = function(win_height, win_width)
        local width = vim.api.nvim_get_option('columns')
        local height = vim.api.nvim_get_option('lines')
        local row = math.ceil((height - win_height))
        local col = math.ceil((width - win_width) / 2)
        return row, col
    end,
}

M.loc = {
    "center",
    "bottom_center"
}

local default_loc = "bottom_center"

local function call_cmd(cmd)
end


local function draw(last_query, current_selection, bufnr, winnr, source, sorter, handler)
    local hlns = vim.api.nvim_create_namespace('fuzzy-hl')
    vim.api.nvim_buf_set_lines(bufnr, 0, -2, false, source)
    vim.cmd [[ startinsert! ]]
    vim.api.nvim_buf_add_highlight(bufnr, hlns, 'Statusline', current_selection -1 , 0, -1)
    local up = function()
        vim.api.nvim_buf_clear_namespace(bufnr, hlns, 0, -1)
        draw(last_query, current_selection-1, bufnr, winnr, source, sorter, handler)
    end
    local down = function()
        vim.api.nvim_buf_clear_namespace(bufnr, hlns, 0, -1)
        draw(last_query, current_selection+1, bufnr, winnr, source, sorter, handler)
    end
    -- set keymaps
    vim.keymap.set('i', '<C-n>', down, {
        buffer = bufnr,
    })

    vim.keymap.set('i', '<C-p>', up, {
        buffer = bufnr,
    })


    vim.api.nvim_create_autocmd({'TextChanged', 'TextChangedI'}, {
        pattern = '<buffer>',
        group = vim.api.nvim_create_augroup('fuzzy-updater', {}),
        callback = function()
            local query = vim.api.nvim_buf_get_lines(bufnr, -2, -1, false)[1]
            query = string.sub(query, #vim.fn.prompt_getprompt(bufnr) + 1, #query)
            if query == last_query then
                return
            end
            source = sorter(source)
            draw(query, current_selection, bufnr, winnr, source, sorter, handler)
        end
    })
end

local function fuzzy(opts)
    opts.sorter = opts.sorter or default_sorter
    opts.loc = opts.loc or locations[default_loc]
    opts.query = opts.query or ""
    opts.height = 0.6
    opts.width = 0.5

    if opts.height < 1 then
        opts.height = math.ceil(vim.api.nvim_get_option("lines") * opts.height)
    end

    if opts.width < 1 then
        opts.width = math.ceil(vim.api.nvim_get_option("columns") * opts.width)
    end

    if type(opts.source) == "function" then
        opts.source = opts.source()
    end
    if type(opts.source) == "string" then
        opts.source = call_cmd(opts.source)
    end

    opts.source = opts.sorter(opts.source)

    local row, col = opts.loc(opts.width, opts.height)

    local win_opts = {
      style = 'minimal',
      relative = 'editor',
      width = opts.width,
      height = opts.height,
      row = row,
      col = col,
    }
    local bufnr = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_option(bufnr, 'bufhidden', 'wipe')
    vim.api.nvim_buf_set_option(bufnr, 'buftype', 'prompt')
    local winnr = vim.api.nvim_open_win(bufnr, true, win_opts)
    draw(opts.query, #opts.source, bufnr, winnr, opts.source, opts.sorter, opts.handler)
end

setmetatable(M, {
    __call = function(_, opts)
        fuzzy(opts)
    end
})


fuzzy {
    source = {'a', 'b', 'c', 'd'},
    sorter = function(tbl)
        return tbl
    end,
    handler = function(item)
        print("choosen .. " .. item)
    end
}

return M
