set fish_greeting ""

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

alias snappvpn='sudo openfortivpn -c ~/snappDC.conf'

alias gs='git status'
alias gd='git diff'
alias gp='git push'
function gpsup
	git push --set-upstream origin {$git_current_branch}
end

function fish_prompt
    printf '%s@%s%s%s > ' $USER \
        (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
end

if command -v nvim &> /dev/null
    alias vim='nvim'
    export EDITOR='nvim'
end

function reload
	source $HOME/.config/fish/config.fish
end


alias ca='cargo'

