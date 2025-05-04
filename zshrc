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
    alias vi='nvim'
    alias v='nvim'
fi

if command -v cursor &> /dev/null
then
    alias code='cursor'
fi

if ! command -v starship &> /dev/null
then
    curl -sS https://starship.rs/install.sh | sh
fi

eval "$(starship init zsh)"


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias artisan='php artisan'
alias wip='git add .; git commit -m "$(date +"%Y-%m-%d %H:%M:%S")"; git push origin $(git_current_branch)'
alias gwip='git add .; git commit -m "$(date +"%Y-%m-%d %H:%M:%S")"; git push origin $(git_current_branch)'

export PATH="/Users/amirrezaask/.config/herd-lite/bin:$PATH"
export PHP_INI_SCAN_DIR="/Users/amirrezaask/.config/herd-lite/bin:$PHP_INI_SCAN_DIR"
export PATH="$HOME/go/bin:$PATH"

export EDITOR='nvim'
export GIT_EDITOR="$EDITOR"

[[ ! -r '/Users/amirrezaask/.opam/opam-init/init.zsh' ]] || source '/Users/amirrezaask/.opam/opam-init/init.zsh' > /dev/null 2> /dev/null
