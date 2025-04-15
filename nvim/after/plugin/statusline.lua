---@class StatusLineSection
---@field display fun(): string

---@param section StatusLineSection
local function HighlightedSection(section, hl)
    return {
        display = function()
            return "%#" .. hl .. "#" .. section.display() .. "%#StatusLine#"
        end
    }
end

---@return StatusLineSection
local ModeSection = {
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

local FileSection = '%f'
local LineSection = '%l'
local ColumnSection = '%c'
local SeperatorSection = '%='
local FileTypeSection = '%y'

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

StatusLine = make_statusline({
    HighlightedSection(ModeSection, 'DiffText'),
    SeperatorSection,
    FileSection,
    SeperatorSection,
    '[',
    LineSection,
    ':',
    ColumnSection,
    ']',
})

vim.o.statusline = "%!v:lua.StatusLine()"
