return function()
    local nvim_statusline = require("nvim-statusline")
    local sections = nvim_statusline.sections
    nvim_statusline.setup {
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
end
