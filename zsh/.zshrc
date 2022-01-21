source $HOME/.profile

source $DOTFILES/zsh/antigen.zsh

antigen use oh-my-zsh

antigen bundle git

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions

command -v 'starship' > /dev/null

if [ "$?" != '0' ]; then
    sh -c "$(curl -fsSL https://starship.rs/install.sh)"
fi

command -v 'oh-my-posh' > /dev/null

if [ "$?" != '0' ]; then
    sudo wget https://github.com/JanDeDobbeleer/oh-my-posh/releases/latest/download/posh-linux-amd64 -O /usr/local/bin/oh-my-posh
    sudo chmod +x /usr/local/bin/oh-my-posh
    mkdir ~/.poshthemes
    wget https://github.com/JanDeDobbeleer/oh-my-posh/releases/latest/download/themes.zip -O ~/.poshthemes/themes.zip
    unzip ~/.poshthemes/themes.zip -d ~/.poshthemes
    chmod u+rw ~/.poshthemes/*.json
    rm ~/.poshthemes/themes.zip
fi

antigen apply

eval "$(oh-my-posh --init --shell zsh --config ~/.poshthemes/craver.omp.json)"

# eval "$(starship init zsh)"

