---@class StatusLineSection
---@field display fun(): string

local sections = {}

---@param section StatusLineSection
function sections.HighlightedSection(section, hl)
    return {
        display = function()
            return "%#" .. hl .. "#" .. section.display() .. "%#StatusLine#"
        end
    }
end

---@return StatusLineSection
sections.ModeSection = {
    display = function()
        local mode = vim.api.nvim_get_mode().mode
        local mode_map = {
            ['n'] = 'Normal',
            ['i'] = 'Insert',
            ['v'] = 'Visual',
            ['V'] = 'Visual Line',
            ['\22'] = 'Visual Block', -- \22 is Ctrl-V
            ['c'] = 'Command',
            ['R'] = 'Replace',
            ['s'] = 'Select',
            ['S'] = 'Select Line',
            ['\19'] = 'Select Block', -- \19 is Ctrl-S
            ['t'] = 'Terminal',
            ['no'] = 'Operator Pending',
            ['niI'] = 'Normal (Insert)',
            ['niR'] = 'Normal (Replace)',
            ['niV'] = 'Normal (Virtual Replace)',
            ['nt'] = 'Normal (Terminal)',
            ['rm'] = 'More Prompt',
            ['r?'] = 'Confirm',
            ['!'] = 'Shell'
        }
        mode = mode_map[mode] or 'Unknown'
        return '[' .. mode .. ']'
    end
}
local function make_format_section(char)
    return {
        display = function()
            return char
        end
    }
end

local home = vim.env.HOME
local function shorten_path_elipsis(path, cwd)
    local THRESHOLD = 40 -- Default from snacks.nvim picker.util: smart path truncate
    if not path or path == "" then
        return ""
    end

    -- Normalize cwd: ensure it ends with a slash and handle nil
    cwd = cwd or ""
    if cwd ~= "" and cwd:sub(-1) ~= "/" then
        cwd = cwd .. "/"
    end

    -- Get home directory from environment
    if home ~= "" and home:sub(-1) ~= "/" then
        home = home .. "/"
    end

    -- Remove home prefix from path if present, then cwd if it follows
    local relative_path = path
    if home ~= "" and path:sub(1, #home) == home then
        relative_path = "~/" .. path:sub(#home + 1)
        -- If cwd includes home, adjust cwd to remove home as well
        if cwd:sub(1, #home) == home then
            cwd = cwd:sub(#home + 1)
        end
    end

    -- Remove cwd prefix from the remaining path if present
    if cwd ~= "" and relative_path:sub(1, #cwd) == cwd then
        relative_path = relative_path:sub(#cwd + 1)
    end

    -- If the resulting path is shorter than threshold, return it
    if #relative_path <= THRESHOLD then
        return relative_path
    end

    local parts = {}
    for part in relative_path:gmatch("[^/]+") do
        table.insert(parts, part)
    end

    -- If no parts or just one part, return the relative path
    if #parts <= 1 then
        return relative_path
    end

    -- Keep the first part, parent directory, and file name
    local first_part = parts[1] or ""
    local parent = #parts > 1 and parts[#parts - 1] or ""
    local filename = parts[#parts] or ""

    -- Build the shortened path with ellipsis, mimicking snacks.nvim
    local shortened
    if #parts <= 2 then
        -- If only two parts (e.g., "dir/file"), no ellipsis needed
        shortened = first_part .. "/" .. filename
    else
        -- Include first part, ellipsis, parent, and file
        shortened = first_part .. "/…/" .. parent .. "/" .. filename
        -- If still too long, trim further by keeping only first and last parts
        if #shortened > THRESHOLD then
            shortened = first_part .. "/…/" .. filename
        end
    end

    return shortened
end

local function fish_shorten_path(path, threshold)
    -- Default threshold to 20 if not provided
    threshold = threshold or 20

    -- If path is empty or nil, return empty string
    if not path or path == "" then
        return ""
    end

    if home ~= "" and home:sub(-1) ~= "/" then
        home = home .. "/"
    end
    local cwd = vim.fn.getcwd()

    if home ~= "" and path:sub(1, #home) == home then
        path = "~" .. path:sub(#home)
        if cwd:sub(1, #home) == home then
            cwd = cwd:sub(#home + 1)
        end
    end
    -- Split path into components using forward slash
    local parts = {}
    for part in path:gmatch("[^/]+") do
        table.insert(parts, part)
    end

    -- Handle absolute paths
    local is_absolute = path:sub(1, 1) == "/"

    -- If it's just a single component or empty, return as is
    if #parts <= 1 then
        return path
    end

    -- Build the shortened path
    local result = {}
    for i, part in ipairs(parts) do
        if i == #parts then
            -- Last component keeps full name
            table.insert(result, part)
        else
            -- Take first character of intermediate directories
            table.insert(result, part:sub(1, 1))
        end
    end

    -- Join components and add leading slash if original was absolute
    local shortened = table.concat(result, "/")
    if is_absolute then
        shortened = "/" .. shortened
    end

    -- Return original path if shortened version is below threshold
    if #shortened < threshold then
        return path
    end

    return shortened
end


sections.FileSection = function(opts)
    opts = opts or {}
    opts.shorten_style = opts.shorten_style or 'fish'

    return {
        display = function()
            local buf = vim.api.nvim_get_current_buf()
            local path = vim.api.nvim_buf_get_name(buf)
            if opts.shorten_style == 'fish' then
                path = fish_shorten_path(path)
            elseif opts.shorten_style == 'elipsis' then
                path = shorten_path_elipsis(path)
            end

            return path
        end

    }
end

sections.LineSection = '%l'
sections.ColumnSection = '%c'
sections.SeperatorSection = '%='
sections.FileTypeSection = '%y'

---@param sections table<StatusLineSection | string>
local function make_statusline(sections)
    return function()
        local out = ""
        for _, s in ipairs(sections) do
            if type(s) == 'table' then
                out = out .. s.display()
            elseif type(s) == 'string' then
                out = out .. make_format_section(s).display()
            end
        end

        return out
    end
end


_G.___NVIM_STATUSLINE = make_statusline {
    sections.HighlightedSection(sections.ModeSection, 'DiffText'),
    sections.SeperatorSection,
    sections.FileSection { shorten_style = 'elipsis' },
    sections.SeperatorSection,
    '[',
    sections.LineSection,
    ':',
    sections.ColumnSection,
    ']',
}


vim.o.statusline = "%!v:lua.___NVIM_STATUSLINE()"
