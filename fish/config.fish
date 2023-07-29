alias v='nvim'
alias gs='git status'
alias gd='git diff'

set -x GO111MODULE 'on'
set -x GOPATH "$HOME"
set -x GOPRIVATE 'gitlab.snapp.ir'
set -x GOPROXY 'goproxy.io,direct'
set -x EDITOR 'vim'

set -x PATH "$HOME/.local/bin:$PATH"
set -x PATH "$HOME/.cargo/bin:$PATH"
set -x PATH "$GOROOT/bin:$PATH"
set -x PATH "$HOME/.config/composer/vendor/bin:$PATH"
set -x PATH "/opt/homebrew/bin:$PATH"
set -x PATH "$GOPATH/bin:$PATH"
set -x PATH "/Users/amirreza/Library/Python/3.10/bin:$PATH"
set -x PATH "/Applications/Emacs.app/Contents/MacOS:$PATH"
set -x PATH "$PATH:$HOME/dev/dotfiles/bin"


set -x __fish_git_prompt_show_informative_status 'yes'
set -x __fish_git_prompt_showuntrackedfiles 'yes'
set -x __fish_git_prompt_showdirtystate 'yes'
set -x __fish_git_prompt_use_informative_chars 'yes'

function fish_prompt
    printf '%s %s ' (set_color $fish_color_cwd) (prompt_pwd) (set_color red) (fish_vcs_prompt)
end

