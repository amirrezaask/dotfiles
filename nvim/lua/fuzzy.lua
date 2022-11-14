-- Simple Fuzzy finder
local M = {}

local lev = require'levenshtein'

local default_sorter = lev.match
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

function table.slice(tbl, first, last, step)
  local sliced = {}

  for i = first or 1, last or #tbl, step or 1 do
    sliced[#sliced+1] = tbl[i]
  end

  return sliced
end

local default_loc = "bottom_center"

local function call_cmd(cmd)
    local output = vim.fn.system(cmd)
    return vim.split(output, '\n')
end


local function draw(last_query, last_window, current_selection, bufnr, winnr, original_source, source, sorter, handler, live)
    local hlns = vim.api.nvim_create_namespace('fuzzy-hl')
    vim.api.nvim_buf_set_lines(bufnr, 0, -2, false, source)
    vim.cmd [[ startinsert! ]]
    vim.api.nvim_buf_add_highlight(bufnr, hlns, 'CursorLineNr', current_selection -1 , 0, -1)
    local up = function()
        vim.api.nvim_buf_clear_namespace(bufnr, hlns, 0, -1)
        current_selection = current_selection-1
        if current_selection < 1 then
            current_selection=1
        end
        draw(last_query, last_window, current_selection, bufnr, winnr, original_source, source, sorter, handler)
    end

    local down = function()
        vim.api.nvim_buf_clear_namespace(bufnr, hlns, 0, -1)

        current_selection = current_selection+1
        if current_selection > #source - 1 then
            current_selection= #source
        end
        draw(last_query, last_window, current_selection, bufnr, winnr, original_source ,source, sorter, handler)
    end

    local enter = function()
        vim.api.nvim_buf_clear_namespace(bufnr, hlns, 0, -1)
        local line = vim.api.nvim_buf_get_lines(bufnr, current_selection-1, current_selection, false)[1]
        vim.api.nvim_set_current_win(last_window)
        vim.api.nvim_win_close(winnr, true)
        handler(line)
    end
    local close = function()
        vim.api.nvim_buf_clear_namespace(bufnr, hlns, 0, -1)
        vim.api.nvim_set_current_win(last_window)
        vim.api.nvim_win_close(winnr, true)
    end
    -- set keymaps
    vim.keymap.set('i', '<C-n>', down, {
        buffer = bufnr,
    })

    vim.keymap.set('i', '<C-p>', up, {
        buffer = bufnr,
    })

    vim.keymap.set('i', '<CR>', enter, {
        buffer = bufnr
    })

    vim.keymap.set('i', '<C-c>', close, {
        buffer = bufnr
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
            source = sorter(query, source)
            draw(query, last_window, #source, bufnr, winnr, original_source, source, sorter, handler, live)
        end
    })
end

-- @function
local function fuzzy(opts)
    opts.sorter = opts.sorter or default_sorter
    opts.loc = opts.loc or locations[default_loc]
    opts.query = opts.query or ""
    opts.width = 0.4
    opts.live = opts.live or false

    if type(opts.source) == "function" then
        opts.original_source = opts.source
        opts.source = opts.source()
    end
    if type(opts.source) == "string" then
        opts.source = call_cmd(opts.source)
    end

    if opts.width < 1 then
        opts.width = math.ceil(vim.api.nvim_get_option("columns") * opts.width)
    end

    opts.height = #opts.source+1
    opts.source = opts.sorter(opts.query, opts.source)

    local row, col = opts.loc(opts.height, opts.width)

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
    local last_window = vim.api.nvim_get_current_win()
    local winnr = vim.api.nvim_open_win(bufnr, true, win_opts)
    vim.api.nvim_win_set_option(winnr, 'winhl', 'Normal:Statusline')
    draw(opts.query, last_window, #opts.source, bufnr, winnr, opts.original_source, opts.source, opts.sorter, opts.handler, opts.live)
end

setmetatable(M, {
    __call = function(_, opts)
        fuzzy(opts)
    end
})
function M.rg()
    fuzzy {
        source = function()
            local output = call_cmd('rg --color never ""')
            return table.slice(output, 1, #output-1)
        end,
        handler = function(item)
            print(item)
        end
    }
end


function M.rg_files()
    fuzzy {
        source = function()
            local output = call_cmd('rg --files --color never')
            return table.slice(output, 1, #output-1)
        end,
        handler = function(item)
            vim.cmd ([[ edit ]] .. item)
        end
    }
end

return M
