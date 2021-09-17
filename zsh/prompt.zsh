# TODO: Check if starship is set as prompt do these


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
    eval "$(oh-my-posh --init --shell zsh --config ~/src/github.com/amirrezaask/dotfiles/powershell/amirreza.omp.json)"
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
fi



