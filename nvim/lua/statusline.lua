-- express line
local el = require('el')
local extensions = require('el.extensions')
local builtin = require('el.builtin')
local sections = require('el.sections')
local lsp_statusline = require('el.plugins.lsp_status')
local subscribe = require('el.subscribe')

el.setup{
  generator = function(win_id)
    return {
      extensions.mode,
     '[', builtin.line, ' : ',  builtin.column, ']',
     '[', extensions.git_branch ,']',
      sections.split,
      subscribe.buf_autocmd("el_file_icon", "BufRead", function(_, bufnr)
        local icon = extensions.file_icon(_, bufnr)
        if icon then
          return icon .. ' '
        end
        return ''
      end),
      builtin.file,
      sections.collapse_builtin {
        ' ',
        builtin.modified_flag
      },
      sections.split,
      lsp_statusline.segment,
      lsp_statusline.current_function,
      subscribe.buf_autocmd(
        "el_git_status",
        "BufWritePost",
        function(window, buffer)
          return extensions.git_changes(window, buffer)
        end
      ),
     '[', builtin.line, ' : ',  builtin.column, ']',
      sections.collapse_builtin{
        '[',
          builtin.help_list,
          builtin.readonly_list,
        ']',
    },
    builtin.filetype,
    }
  end
}


