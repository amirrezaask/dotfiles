if ! test -d $HOME/.oh-my-zsh 
then
    git clone https://github.com/ohmyzsh/ohmyzsh.git ~/.oh-my-zsh
fi

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(git)

source $ZSH/oh-my-zsh.sh

reload() {
    source ~/.zshrc
}

if command -v nvim &> /dev/null
then
    alias vim='nvim'
fi

if command -v cursor &> /dev/null
then
    if ! command -v code &> /dev/null
    then
        alias code='cursor'
    fi
fi

if ! command -v starship &> /dev/null
then
    curl -sS https://starship.rs/install.sh | sh
fi

eval "$(starship init zsh)"


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias artisan='php artisan'
alias gwip='echo "$(date)"'

alias gwip='git add . && git commit -m "Work In Progress $(date +"%Y-%m-%d %H:%M:%S")" && git push origin $(git_current_branch)'
alias gp='git push origin $(git_current_branch)'
alias gP='git pull --rebase'
alias gs='git status'
alias nah='git stash'
alias ok='git add . && git commit'

export PATH="/Users/amirrezaask/.config/herd-lite/bin:$PATH"
export PHP_INI_SCAN_DIR="/Users/amirrezaask/.config/herd-lite/bin:$PHP_INI_SCAN_DIR"

export EDITOR='nvim'
# export EDITOR='code -w'
export GIT_EDITOR="$EDITOR"


# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
[[ ! -r '/Users/amirrezaask/.opam/opam-init/init.zsh' ]] || source '/Users/amirrezaask/.opam/opam-init/init.zsh' > /dev/null 2> /dev/null
# END opam configuration
