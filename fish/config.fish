export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$HOME/.local/bin"
function fish_prompt -d "Write out the prompt"
    printf '%s %s%s%s > ' $USER \
        (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
end
export EDITOR='nvim'
export GIT_EDITOR='nvim'
alias rn='zellij action rename-tab '
