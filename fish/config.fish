set fish_greeting "" # no start greetings

set -gx GO111MODULE 'on'
set -gx GOPATH "$HOME"
set -gx GOPRIVATE "gitlab.snapp.ir"
set -gx GOPROXY 'goproxy.io,direct'

set -gx PATH "/opt/homebrew/bin:$PATH"

set -gx PATH "$HOME/.local/bin:$PATH"

set -gx PATH "$GOROOT/bin:$PATH"

set -gx PATH "$HOME/.config/composer/vendor/bin:$PATH"

set -gx PATH "$GOPATH/bin:$PATH"

set -gx PATH "$HOME/.cargo/bin:$PATH"

eval $(brew shellenv)

set -gx EDITOR 'vim'
set -gx HOMEBREW_NO_AUTO_UPDATE '1'

set -gx FZF_DEFAULT_OPTS '--height 20%'
set -gx FZF_DEFAULT_COMMAND 'rg --files'

alias snappvpn 'sudo openfortivpn -c ~/snappDC.conf'

function tmux-open
    tmux new-session -A -t (realpath $argv[1]) -c (realpath $argv[1])
end

alias o 'tmux-open'

# Git stuff
alias gs 'git status'
alias gd 'git diff'
alias gp 'git push'

alias reload 'source ~/.config/fish/config.fish'

# git prompt settings
set -g __fish_git_prompt_show_informative_status 1
set -g __fish_git_prompt_showdirtystate 'yes'
set -g __fish_git_prompt_char_stateseparator ' '
set -g __fish_git_prompt_char_dirtystate "✖"
set -g __fish_git_prompt_char_cleanstate "✔"
set -g __fish_git_prompt_char_untrackedfiles "…"
set -g __fish_git_prompt_char_stagedstate "●"
set -g __fish_git_prompt_char_conflictedstate "+"
set -g __fish_git_prompt_color_dirtystate yellow
set -g __fish_git_prompt_color_cleanstate green --bold
set -g __fish_git_prompt_color_invalidstate red
set -g __fish_git_prompt_color_branch cyan --dim --italics
