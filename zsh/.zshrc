export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="robbyrussell"
# git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
# git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
plugins=(git zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

export GO111MODULE='on'
export GOPATH="$HOME"
export GOPRIVATE='gitlab.snapp.ir'
# export GOPROXY='https://repo.snapp.tech/repository/goproxy,goproxy.io,direct'
export GOPROXY='goproxy.io,direct'
export EDITOR='vim'
export OSS="$HOME/personal"
export DOTFILES="$HOME/dev/dotfiles"
export SNAPP="$HOME/work/snapp"
export ESPAD="$HOME/work/espad"
export PLAN9=/Users/amirreza/plan9 
export PATH="$HOME/.emacs.d/bin/:/Applications/Emacs.app/Contents/MacOS:$GOPATH/bin:/opt/homebrew/bin:$ELIXIR/bin:$HOME/.luarocks/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$HOME/.cargo/bin:$HOME/.local/bin:/usr/local/Postman:$HOME/.local/elixir-ls/:$HOME/.cache/rebar3/bin:$PATH:$DOTFILES/bin:$HOME/.composer/vendor/bin:$PLAN9/bin:$DOTFILES/acme-bin"

# Aliases
alias reload='source ~/.zshrc'

alias gs='git status'

alias snappvpn='sudo openfortivpn -c ~/snappDC.conf'
alias luamake=/home/amirreza/.local/lua-language-server/3rd/luamake/luamake

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

if command -v nvim &> /dev/null
then
    alias v='nvim'
    alias vi='nvim'
    alias vim='nvim'
    export EDITOR='nvim'
fi

if command -v exa &> /dev/null
then
    alias ls='exa'
    alias ll='exa -la'
    alias l='exa -la'
fi

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

export HOMEBREW_NO_AUTO_UPDATE=1

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_OPTS='--height 20%'
export FZF_DEFAULT_COMMAND='rg --files'

subl() {
   SUBL_BIN='' 
   /Applications/Sublime\ Text.app/Contents/MacOS/sublime_text $1 > /dev/null 2>&1 &
}

alias ta='tmux attach -t '
alias tl='tmux list-sessions'
alias tns='tmux new-session -s '
alias dots='cd ~/dev/dotfiles'


ss_proxy() {
    export http_proxy='http://localhost:1087'
    export https_proxy='http://localhost:1087'
}



alias acme-laptop="acme -f /mnt/font/'JetBrainsMono-Regular'/14a/font"
alias acme-monitor="acme -f /mnt/font/'JetBrainsMono-Regular'/18a/font"

