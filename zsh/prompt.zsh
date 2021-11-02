if [ "$PROMPT_ENGINE" = "starship" ]; then
    if ! command -v starship &> /dev/null
    then
        sh -c "$(curl -fsSL https://starship.rs/install.sh)"
    fi
    eval "$(starship init zsh)"
elif [ "$PROMPT_ENGINE" = "oh-my-posh" ]; then
    if ! command -v oh-my-posh &> /dev/null
    then
        sudo wget https://github.com/JanDeDobbeleer/oh-my-posh/releases/latest/download/posh-linux-amd64 -O /usr/local/bin/oh-my-posh
        sudo chmod +x /usr/local/bin/oh-my-posh
    fi
    if [ ! -d ~/.poshthemes ]; then
        mkdir -p ~/.poshthemes
        wget https://github.com/JanDeDobbeleer/oh-my-posh/releases/latest/download/themes.zip -O ~/.poshthemes/themes.zip
        unzip ~/.poshthemes/themes.zip -d ~/.poshthemes
        chmod u+rw ~/.poshthemes/*.json
        rm ~/.poshthemes/themes.zip
    fi
    eval "$(oh-my-posh --init --shell zsh --config ~/.poshthemes/craver.omp.json)"

elif [ "$PROMPT_ENGINE" = "omz" ]; then
    antigen theme robbyrussell
else
    get_branch(){
        branch=$(parse_git_branch)
        if [ -z "$branch" ]; then
            echo ""
        else
            echo " $branch"
        fi
    }

    setopt PROMPT_SUBST

    PROMPT='%F{46}%~%f$(get_branch) > '
    PROMPT='%F{#bcbcbc}[$(date "+%H:%M")]%f %F{#005fff}`hostname`%f %F{#d7d700}%~%f %F{#00d700}$(get_branch)%f %F{red}| %f'
fi



