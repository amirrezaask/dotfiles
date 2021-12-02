ZSH_CONFIG_BASE_DIR="$HOME/.config/zsh"
source "${ZSH_CONFIG_BASE_DIR}/antigen.zsh"

antigen use oh-my-zsh
antigen bundle git
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions

source ~/env

source "${ZSH_CONFIG_BASE_DIR}/vars.zsh"

source "$ZSH_CONFIG_BASE_DIR/git.zsh"

export PROMPT_ENGINE="custom"

get_branch(){
    branch=$(parse_git_branch)
    if [ -z "$branch" ]; then
        echo ""
    else
        echo " $branch"
    fi
}

setopt PROMPT_SUBST
NEWLINE=$'\n'
PROMPT="%F{#bcbcbc}[$(date '+%H:%M')]%f %F{#005fff}`hostname`%f %F{#d7d700}%~%f %F{#00d700}$(parse_git_branch)%f${NEWLINE}%F{red}|> %f"

antigen apply
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias luamake=/home/amirreza/.local/lua-language-server/3rd/luamake/luamake
